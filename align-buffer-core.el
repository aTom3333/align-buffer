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

PROPERTIES is a plist the consumer keeps its own state in.  One key is read
from it: `:owned-buffers', a list of buffers to kill when the session ends,
for a consumer that made a buffer to show."
  left-buffer
  right-buffer
  left-index
  right-index
  left-point
  right-point
  windows
  properties)


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

(defun align-buffer-session-live-p (session)
  "Return non-nil when both of SESSION's buffers are still alive."
  (and (buffer-live-p (align-buffer-session-left-buffer session))
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

(defun align-buffer-line-position (buffer line)
  "Return where LINE begins in BUFFER, counting from one and ignoring narrowing.
A LINE past the end gives the last line, so a correspondence left stale by an
edit still places the window somewhere rather than failing."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (save-excursion
          (goto-char (point-min))
          (forward-line (1- (max 1 line)))
          (point))))))

(defun align-buffer-line-at (buffer position)
  "Return the line number of POSITION in BUFFER, ignoring narrowing."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (line-number-at-pos position t))))


;;; Windows

(defun align-buffer-effective-margin (window)
  "Return the rows `scroll-margin' actually keeps clear in WINDOW."
  (let ((height (window-body-height window)))
    (min (buffer-local-value 'scroll-margin (window-buffer window))
         (max 0 (truncate (* maximum-scroll-margin height))))))

(provide 'align-buffer-core)

;;; align-buffer-core.el ends here
