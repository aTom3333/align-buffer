;;; align-buffer-core.el --- Types and the line correspondence -*- lexical-binding: t; -*-

;;; Commentary:

;; The session, and the map between a line of one buffer and the line of the
;; other it stands level with.
;;
;; A correspondence is a set of (LEFT-LINE . RIGHT-LINE) pairs and nothing
;; more.  Three things it is not:
;;
;;   not monotonic  a block that moved pairs its old lines with new ones far
;;                  away, so the right lines are in no particular order.
;;   not total      a line with no counterpart is simply absent.
;;   not one to one several lines may pair with the same line facing them.
;;
;; Every question the sync asks is "which pair governs this line", and the
;; answer is the pair with the greatest line at or below it.  Each side
;; therefore gets its own index sorted by its own line: pairs sorted by the
;; left line are in no useful order for a question about the right one.

;;; Code:

(require 'cl-lib)
(require 'seq)

(defgroup align-buffer nil
  "Show two buffers side by side with corresponding lines level."
  :group 'convenience
  :prefix "align-buffer-")


;;; The session

(cl-defstruct (align-buffer-session (:constructor align-buffer-session-create)
                                    (:copier nil))
  "Two buffers kept scrolled to each other, and what corresponds between them.

LEFT-BUFFER and RIGHT-BUFFER are the buffers themselves, and they are the
session's identity.  LEFT-INDEX and RIGHT-INDEX are vectors of (OWN . OTHER)
line pairs sorted by OWN, one for asking from each side.  LEFT-POINT and
RIGHT-POINT are where point was when the session was built.  WINDOWS are the
windows the layout put the buffers in.

SECTIONS are the stretches of lines the consumer calls interesting, which is
what `align-buffer-next-section' and `align-buffer-previous-section' move
between.

PROPERTIES is a plist the consumer keeps its own state in.  One key is read
from it: `:owned-buffers', a list of buffers to kill when the session ends,
for a consumer that made a buffer to show.

TORN-DOWN is set when the session is taken apart, which is what makes a
handle kept across that answer for itself."
  left-buffer
  right-buffer
  left-index
  right-index
  left-point
  right-point
  windows
  sections
  properties
  torn-down)


;;; Buffer-local state

(defvar-local align-buffer--session nil
  "The session this buffer belongs to.")

(defvar-local align-buffer--side nil
  "Which side of its session this buffer is, `left' or `right'.")


;;; The registry

(defvar align-buffer--sessions nil
  "Every live session.")

(defun align-buffer-sessions ()
  "Return the live sessions, forgetting any that have died."
  (setq align-buffer--sessions
        (seq-filter #'align-buffer-session-live-p align-buffer--sessions)))

(defun align-buffer-session-gone-p (session)
  "Return non-nil when SESSION has been taken apart, or was made before this.

A handle is held in timers and hooks, so rebuilding the package while a
session is up leaves one behind with fewer slots than a session has now.  Such
a handle belongs to code that is no longer here, so it reads as a session that
has gone: the timer holding it stops on its own, where asking it for a slot it
has not got would signal every time it ran."
  (condition-case nil
      (align-buffer-session-torn-down session)
    (args-out-of-range t)))

(defun align-buffer-session-live-p (session)
  "Return non-nil while SESSION is up: not taken apart, and both buffers alive.

Both buffers outlive an ordinary session, a reader's own file buffers among
them, so what a handle kept across a teardown has to answer is whether the
session is still there, not whether its two buffers are."
  (and (not (align-buffer-session-gone-p session))
       (buffer-live-p (align-buffer-session-left-buffer session))
       (buffer-live-p (align-buffer-session-right-buffer session))))

(defun align-buffer-current-session ()
  "Return the session of the current buffer, or nil when it is in none."
  (and align-buffer--session
       (align-buffer-session-live-p align-buffer--session)
       align-buffer--session))

(defun align-buffer-session-for (buffer)
  "Return the live session BUFFER belongs to, or nil."
  (and (buffer-live-p buffer)
       (let ((session (buffer-local-value 'align-buffer--session buffer)))
         (and session (align-buffer-session-live-p session) session))))

(defun align-buffer-side ()
  "Return which side of its session this buffer is, `left' or `right'."
  align-buffer--side)

(defun align-buffer-other-side (side)
  "Return the side facing SIDE."
  (if (eq side 'left) 'right 'left))

(defun align-buffer-buffer (session side)
  "Return SESSION's buffer for SIDE."
  (if (eq side 'left)
      (align-buffer-session-left-buffer session)
    (align-buffer-session-right-buffer session)))

(defun align-buffer-index (session side)
  "Return SESSION's correspondence index for asking from SIDE."
  (if (eq side 'left)
      (align-buffer-session-left-index session)
    (align-buffer-session-right-index session)))


;;; Building the correspondence

(defun align-buffer-check-pairs (pairs)
  "Signal unless every element of PAIRS is a cons of two line numbers."
  (dolist (pair (append pairs nil))
    (unless (and (consp pair)
                 (integerp (car pair)) (> (car pair) 0)
                 (integerp (cdr pair)) (> (cdr pair) 0))
      (error "Not a pair of line numbers: %S" pair))))

(defun align-buffer-build-index (pairs side)
  "Return PAIRS as a vector of (OWN . OTHER) sorted by OWN, asked from SIDE.
PAIRS is a list or a vector of (LEFT-LINE . RIGHT-LINE)."
  (let ((entries (mapcar (if (eq side 'left)
                             (lambda (pair) (cons (car pair) (cdr pair)))
                           (lambda (pair) (cons (cdr pair) (car pair))))
                         (append pairs nil))))
    ;; A stable sort, so pairs sharing a line keep the order they were given.
    (vconcat (sort entries (lambda (one other) (< (car one) (car other)))))))


;;; Asking the correspondence

(defun align-buffer-anchor (session side line)
  "Return the pair of SESSION governing LINE on SIDE, as (OWN . OTHER).

The pair with the greatest OWN at or below LINE.  When LINE sits above every
pair there is none, and the first pair is returned instead; its OWN is then
above LINE, which is how the two cases are told apart.  Nil when SIDE has no
pairs at all.

Where several pairs share one OWN, the last one given decides."
  (let ((index (align-buffer-index session side)))
    (when (and index (> (length index) 0))
      (if (> (car (aref index 0)) line)
          (aref index 0)
        (let ((low 0)
              (high (1- (length index))))
          (while (< low high)
            (let ((middle (/ (+ low high 1) 2)))
              (if (<= (car (aref index middle)) line)
                  (setq low middle)
                (setq high (1- middle)))))
          (aref index low))))))

(defun align-buffer-corresponding-line (session side line)
  "Return the line facing SIDE's LINE in SESSION, or nil when nothing does."
  (let ((anchor (align-buffer-anchor session side line)))
    (and anchor (<= (car anchor) line) (cdr anchor))))


;;; Lines and positions

(defvar-local align-buffer--line-cache nil
  "Where a line was last found in this buffer, as (MARKER LINE TICK).")

(defun align-buffer--line-known ()
  "Return a line we know the place of, as (POSITION . LINE), or nil.
Nil once the buffer's text has changed, since a marker keeps its place in the
text while the line numbering around it moves."
  (pcase align-buffer--line-cache
    (`(,marker ,line ,tick)
     (and (eq tick (buffer-chars-modified-tick))
          (marker-position marker)
          (cons (marker-position marker) line)))))

(defun align-buffer--line-remember (position line)
  "Remember that LINE begins at POSITION."
  (let ((marker (or (car align-buffer--line-cache) (make-marker))))
    (set-marker marker position)
    (setq align-buffer--line-cache
          (list marker line (buffer-chars-modified-tick)))))

(defun align-buffer-line-position (buffer line)
  "Return where LINE begins in BUFFER, counting from one and ignoring narrowing.
A LINE past the end gives the last line, so a correspondence left stale by an
edit still places the window somewhere rather than failing."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (save-excursion
          ;; Walked from the line we found last, which is a few lines away
          ;; while the reader scrolls, rather than from the first line, which
          ;; is as far away as they have read.
          (let* ((wanted (max 1 line))
                 (known (align-buffer--line-known))
                 (left (if known
                           (progn (goto-char (car known))
                                  (forward-line (- wanted (cdr known))))
                         (goto-char (point-min))
                         (forward-line (1- wanted)))))
            (when (zerop left)
              (align-buffer--line-remember (point) wanted))
            (point)))))))

(defun align-buffer-line-at (buffer position)
  "Return the line number of POSITION in BUFFER, ignoring narrowing."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((known (align-buffer--line-known)))
        (if (null known)
            (line-number-at-pos position t)
          ;; Counted from the line we found last, which is near, rather than
          ;; from the first line, which is as far as the reader has read.
          (save-restriction
            (widen)
            (align-buffer--line-from known position)))))))

(defun align-buffer--line-from (known position)
  "Return the line POSITION is on, given KNOWN as (POSITION . LINE)."
  (pcase-let ((`(,from . ,line) known))
    (cond
     ((= position from) line)
     ((> position from)
      ;; `count-lines' counts the line POSITION sits in as well, unless it sits
      ;; at the start of one.
      (let ((lines (count-lines from position)))
        (+ line (if (align-buffer--line-start-p position) lines (1- lines)))))
     (t
      ;; Counting back to a position part way along a line counts whole lines
      ;; either way, since the end of the span is a line start.
      (- line (count-lines position from))))))

(defun align-buffer--line-start-p (position)
  "Return non-nil when POSITION is where a line begins."
  (save-excursion
    (goto-char position)
    (bolp)))


;;; Windows

(defvar-local align-buffer--margin-held nil
  "The reader's `scroll-margin' while this buffer's is held at zero.
Either the number the buffer had of its own, or t where it had none and read
the global one.")

(defun align-buffer-effective-margin (window)
  "Return the rows the scroll margin keeps clear in WINDOW.
The reader's margin even where it is held away from redisplay, so that what
we draw clear of the edges does not change under them."
  (let* ((buffer (window-buffer window))
         (held (buffer-local-value 'align-buffer--margin-held buffer))
         (margin (if (numberp held) held
                   (buffer-local-value 'scroll-margin buffer)))
         (height (window-body-height window)))
    (min margin (max 0 (truncate (* maximum-scroll-margin height))))))

(defun align-buffer-hold-margin (buffer)
  "Keep redisplay from scrolling BUFFER's windows for their margin.

Redisplay scrolls a window whose point sits inside the margin, and the first
pass after a placement is the only one that honours what we wrote: the pass
after it scrolls, and with `scroll-conservatively' at nothing that scroll
recentres.  A placement holding point somewhere the margin dislikes on
purpose therefore has to take the margin out of redisplay's hands, and
`align-buffer-effective-margin' goes on reporting the reader's own."
  (with-current-buffer buffer
    (unless align-buffer--margin-held
      (setq align-buffer--margin-held
            (if (local-variable-p 'scroll-margin) scroll-margin t))
      (setq-local scroll-margin 0))))

(defun align-buffer-release-margin (buffer)
  "Give BUFFER's margin back to redisplay."
  (with-current-buffer buffer
    (when align-buffer--margin-held
      ;; Only where it is still the nothing we left: a reader who has set it
      ;; since keeps what they set.
      (when (equal scroll-margin 0)
        (if (numberp align-buffer--margin-held)
            (setq-local scroll-margin align-buffer--margin-held)
          (kill-local-variable 'scroll-margin)))
      (setq align-buffer--margin-held nil))))

(provide 'align-buffer-core)

;;; align-buffer-core.el ends here
