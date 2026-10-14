;;; align-diff.el --- Align two buffers by their differences -*- lexical-binding: t; -*-

;; Author: Thomas Ferrand
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, vc
;; URL: https://github.com/aTom3333/align-buffer

;;; Commentary:

;; A layer on top of align-buffer that works out what corresponds by diffing.
;; `align-diff-buffers' takes two buffers, runs `diff-command' over them, and
;; hands align-buffer the line pairs it read out of the hunks:
;;
;;   unchanged     every line pairs with the line facing it.
;;   changed       the two sides pair by position, as far as the shorter side
;;                 goes; the lines past that have no counterpart.
;;   one sided     a hunk that is all insertion or all deletion pairs nothing.
;;
;; The colours are overlays in the buffers themselves, at two levels: the lines
;; a hunk covers, and the characters that differ inside it.  Nothing is copied
;; anywhere, so the text under them is the reader's own buffer with its mode,
;; its font lock and its editing intact.
;;
;; Editing is therefore allowed, and moves the diff: once both buffers have
;; been left alone for `align-diff-idle-delay', the diff is run again, the
;; colours are painted afresh, and the windows are brought level again if the
;; pairing itself changed.  `align-diff-refresh' does the same on demand.

;;; Code:

(require 'cl-lib)
(require 'align-buffer)
(require 'diff)
(require 'smerge-mode)

(defgroup align-diff nil
  "Two buffers side by side, aligned by their differences."
  :group 'align-buffer
  :prefix "align-diff-")

(defcustom align-diff-switches '("--strip-trailing-cr")
  "Switches for `diff-command', on top of the unified format align-diff needs."
  :type '(repeat string)
  :group 'align-diff)

(defcustom align-diff-refine-hunks t
  "When non-nil, mark which characters differ inside a hunk."
  :type 'boolean
  :group 'align-diff)

(defcustom align-diff-gap-marker 'slit
  "How a pane is marked where the other side has lines it has none for.
The mark says where, not how many: a line with no counterpart leaves nothing
in the facing pane to colour, and the place it would sit is between two rows.

`slit'    a row a few pixels tall in the gap's colour, which is the one mark
          that takes a row of its own.
`fringe'  a triangle in the fringe, its tip on the boundary between the two
          lines the missing ones belong between.
`pad'     a blank row for each line the other side has, so the two panes read
          line for line.  The rows are display and hold no position of their
          own, and `align-buffer-navigation-mode' is what steps and scrolls
          through them a row at a time.
nil       nothing at all."
  :type '(choice (const :tag "A slit of a row" slit)
                 (const :tag "A triangle in the fringe" fringe)
                 (const :tag "A blank row for each missing line" pad)
                 (const :tag "Nothing" nil))
  :group 'align-diff)

(defcustom align-diff-gap-fringe 'left-fringe
  "Which fringe a gap's triangle goes in."
  :type '(choice (const left-fringe) (const right-fringe))
  :group 'align-diff)

(defcustom align-diff-slit-height 30
  "How tall a slit is drawn, in tenths of a point."
  :type 'integer
  :group 'align-diff)

(defcustom align-diff-idle-delay 0.5
  "Seconds of idleness after which edited buffers are diffed again.
When nil the diff is only run again by `align-diff-refresh'."
  :type '(choice (const :tag "Only when asked for" nil)
                 number)
  :group 'align-diff)

(defface align-diff-removed '((t :inherit diff-removed :extend t))
  "A line the old side alone has, or the old side of a changed line."
  :group 'align-diff)

(defface align-diff-added '((t :inherit diff-added :extend t))
  "A line the new side alone has, or the new side of a changed line."
  :group 'align-diff)

(defface align-diff-slit-added '((t :inherit diff-added :extend t))
  "The band of colour standing for lines the other side has gained.
A slit is a band, so its colour comes from this face's background."
  :group 'align-diff)

(defface align-diff-slit-removed '((t :inherit diff-removed :extend t))
  "The band of colour standing for lines the other side still has."
  :group 'align-diff)

(defface align-diff-gap-added
  '((((class color) (background dark)) :foreground "#5fd75f")
    (((class color) (background light)) :foreground "#1f7a1f")
    (t :inherit diff-added))
  "The triangle standing for lines the other side has gained.
A fringe bitmap is drawn in a face's foreground, so this face sets one where
the diff faces set a background, and it is the brighter for standing alone in
the fringe rather than behind text."
  :group 'align-diff)

(defface align-diff-gap-removed
  '((((class color) (background dark)) :foreground "#ff6c6b")
    (((class color) (background light)) :foreground "#a01010")
    (t :inherit diff-removed))
  "The triangle standing for lines the other side still has."
  :group 'align-diff)

(when (fboundp 'define-fringe-bitmap)
  ;; Two halves of one triangle pointing right, the widest row of it last in
  ;; the upper half and first in the lower.  Hung under the line above the gap
  ;; and over the line below it, the tip falls on the boundary between them
  ;; rather than beside a line, which is where the missing lines belong.
  (define-fringe-bitmap 'align-diff-gap-upper
    [#b10000000 #b11000000 #b11100000 #b11110000
                #b11111000 #b11111100 #b11111110 #b11111111]
    8 8 'bottom)
  (define-fringe-bitmap 'align-diff-gap-lower
    [#b11111110 #b11111100 #b11111000 #b11110000
                #b11100000 #b11000000 #b10000000]
    7 8 'top))

(defface align-diff-refine-removed '((t :inherit diff-refine-removed))
  "The characters a changed line lost."
  :group 'align-diff)

(defface align-diff-refine-added '((t :inherit diff-refine-added))
  "The characters a changed line gained."
  :group 'align-diff)


(defcustom align-diff-navigation t
  "When non-nil, a session's buffers turn on `align-buffer-navigation-mode'.
That is what makes the rows of a pad behave like the lines they stand for:
without it the ordinary motion and scroll commands step over them in one go,
since no position in the buffer falls inside them."
  :type 'boolean
  :group 'align-diff)


;;; Running diff

(defun align-diff--line-count (buffer)
  "Return how many lines BUFFER holds, narrowing or not."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (count-lines (point-min) (point-max)))))

(defun align-diff--unified (old new)
  "Return the diff of buffer OLD against buffer NEW, as a string.
Without context, so that a header gives the changed lines and nothing else:
what lies between two hunks is the same on both sides and pairs up by counting
rather than by reading."
  (let ((output (generate-new-buffer " *align-diff output*"))
        ;; A reader's narrowing is their view of the buffer, not a statement
        ;; about what to compare.
        (diff-entire-buffers t)
        ;; Both buffers written the same way, and with the line endings Emacs
        ;; shows rather than the platform's: on Windows the default write adds a
        ;; carriage return to every line of both temp files, and
        ;; `--strip-trailing-cr' cannot then rescue a side that holds carriage
        ;; returns of its own, since it strips exactly one.
        (coding-system-for-write 'utf-8-emacs-unix))
    (unwind-protect
        (progn
          (diff-no-select old new (append (list "-U0") align-diff-switches)
                          t output)
          (with-current-buffer output
            (buffer-substring-no-properties (point-min) (point-max))))
      (kill-buffer output))))

(defconst align-diff--hunk-header
  "^@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@"
  "Regexp matching a unified hunk header, capturing both ranges.")

(defun align-diff--hunks-in (diff)
  "Return the hunks DIFF reports, as (OLD-LINE OLD-COUNT NEW-LINE NEW-COUNT).
Each line is where the hunk begins on that side, counting from one, and each
count is how many lines of it that side has.

A count the header leaves out is one.  A count of zero comes with the line
before the gap, which is where diff says an insertion goes, so the line is
moved on to where the lines would start had the side any."
  (let ((hunks nil))
    (dolist (line (split-string diff "\n"))
      (when (string-match align-diff--hunk-header line)
        (let ((old-line (string-to-number (match-string 1 line)))
              (old-count (if (match-string 2 line)
                             (string-to-number (match-string 2 line))
                           1))
              (new-line (string-to-number (match-string 3 line)))
              (new-count (if (match-string 4 line)
                             (string-to-number (match-string 4 line))
                           1)))
          (push (list (if (zerop old-count) (1+ old-line) old-line) old-count
                      (if (zerop new-count) (1+ new-line) new-line) new-count)
                hunks))))
    (nreverse hunks)))

(defun align-diff--hunks (old new)
  "Return the hunks between buffer OLD and buffer NEW."
  (align-diff--hunks-in (align-diff--unified old new)))


;;; What corresponds

(defun align-diff--pairs (hunks old new)
  "Return the pairs aligning buffer OLD against buffer NEW, given HUNKS.
A line outside every hunk pairs with the line facing it.  Inside one the two
sides pair by position as far as the shorter side goes, and the lines past that
have no counterpart, which leaves a hunk with lines on one side only pairing
nothing."
  (let ((old-line 1)
        (new-line 1)
        (pairs nil))
    (dolist (hunk hunks)
      (pcase-let ((`(,old-first ,old-count ,new-first ,new-count) hunk))
        ;; The run up to the hunk, which both sides have line for line.
        (while (< old-line old-first)
          (push (cons old-line new-line) pairs)
          (setq old-line (1+ old-line))
          (setq new-line (1+ new-line)))
        (dotimes (index (min old-count new-count))
          (push (cons (+ old-first index) (+ new-first index)) pairs))
        (setq old-line (+ old-first old-count))
        (setq new-line (+ new-first new-count))))
    ;; And the run after the last hunk, which runs out with the shorter side:
    ;; a buffer whose last line has no newline where the other side's has one
    ;; is a line longer by diff's reckoning than by `count-lines'.
    (let ((old-lines (align-diff--line-count old))
          (new-lines (align-diff--line-count new)))
      (while (and (<= old-line old-lines) (<= new-line new-lines))
        (push (cons old-line new-line) pairs)
        (setq old-line (1+ old-line))
        (setq new-line (1+ new-line))))
    (nreverse pairs)))


;;; Colouring the lines

(defun align-diff--after-line (buffer line)
  "Return where the line after LINE begins in BUFFER.
The end of the text when LINE is the last one."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (align-buffer-line-position buffer line))
        (forward-line 1)
        (point)))))

(defun align-diff--mark-run (buffer first count face)
  "Face the COUNT lines of BUFFER from line FIRST with FACE."
  (when (> count 0)
    ;; Over the newline as well, so a face that extends paints the rest of the
    ;; row rather than stopping where the text does.
    (let ((overlay (make-overlay (align-buffer-line-position buffer first)
                                 (align-diff--after-line
                                  buffer (+ first count -1))
                                 buffer)))
      (overlay-put overlay 'align-diff t)
      (overlay-put overlay 'face face)
      ;; Gone once an edit has eaten the line it was on.
      (overlay-put overlay 'evaporate t)
      overlay)))

(defun align-diff--line-end (buffer line)
  "Return where the text of LINE of BUFFER ends.
The last line's end when LINE is past it."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (align-buffer-line-position
                    buffer (min line (align-diff--line-count buffer))))
        (line-end-position)))))

(defun align-diff--end-of-text (buffer)
  "Return where BUFFER's text ends, narrowing or not."
  (with-current-buffer buffer
    (save-restriction (widen) (point-max))))

(defun align-diff--mark-gap-overlay (overlay)
  "Mark OVERLAY as a gap mark of ours."
  (overlay-put overlay 'align-diff t)
  (overlay-put overlay 'align-diff-gap t))

(defun align-diff--mark-gap (buffer line kind &optional rows facing facing-line)
  "Mark BUFFER above LINE, where the other side has lines it has none for.
KIND is `added' where the other side gained them and `removed' where it still
has them, ROWS is how many lines are missing, FACING is the buffer that has
them with FACING-LINE the first of them, and `align-diff-gap-marker' says how
to draw it."
  (pcase align-diff-gap-marker
    ('slit (align-diff--slit buffer line (align-diff--slit-face kind)))
    ('fringe
     (align-diff--fringe-mark buffer line (align-diff--arrow-face kind)))
    ('pad (align-diff--pad buffer line (or rows 1) kind facing facing-line))))


(defun align-diff--pad-face (kind)
  "Return the face a pad standing for KIND's missing lines is drawn in.

The colour of the pane the pad sits in, not of the lines it stands for, so
that the old pane reads as a loss all the way down and the new pane as a
gain, exactly as the coloured lines of the text do.  A pane's gap is named by
what that pane lacks, which puts the two names the other way round."
  (if (eq kind 'added) 'align-diff-removed 'align-diff-added))

(defun align-diff--pad (buffer line rows kind &optional facing facing-line)
  "Give BUFFER ROWS blank rows above LINE, standing for KIND's missing lines.
FACING is the buffer that has those lines and FACING-LINE the first of them."
  (let ((overlay (align-buffer-pad-add
                  buffer
                  ;; Past the last line the rows stand at the end of the text,
                  ;; where a run the other side alone has ends up.
                  (if (> line (align-diff--line-count buffer))
                      (align-diff--end-of-text buffer)
                    (align-buffer-line-position buffer line))
                  rows (align-diff--pad-face kind) facing facing-line)))
    (align-diff--mark-gap-overlay overlay)
    overlay))


(defun align-diff--slit-face (kind)
  "Return the face a slit stands for KIND in.
Its colour comes from a background, a slit being a band of colour, and that is
where the diff faces carry theirs."
  (let ((face (if (eq kind 'added)
                  'align-diff-slit-added
                'align-diff-slit-removed)))
    (list :height align-diff-slit-height
          :background (or (face-background face nil t)
                          (face-foreground face nil t))
          :extend t)))

(defun align-diff--arrow-face (kind)
  "Return the face a triangle stands for KIND in."
  (if (eq kind 'added) 'align-diff-gap-added 'align-diff-gap-removed))

(defun align-diff--slit (buffer line face)
  "Give BUFFER a row a few pixels tall above LINE, drawn in FACE.
A row of its own, so unlike a fringe mark it moves the text below it down, and
no position in the buffer falls inside it."
  (let ((above (1- line)))
    (if (> above 0)
        (align-diff--slit-below buffer above face)
      ;; Nothing above the gap to hang it on, so before the first line it
      ;; goes, which is the one case where the slit takes the row that line's
      ;; number is drawn on.
      (let ((overlay (make-overlay (align-buffer-line-position buffer 1)
                                   (align-buffer-line-position buffer 1)
                                   buffer)))
        (align-diff--mark-gap-overlay overlay)
        (overlay-put overlay 'before-string (propertize "\n" 'face face))
        overlay))))

(defun align-diff--slit-below (buffer line face)
  "Give BUFFER a slit below LINE, drawn in FACE.

Two overlays, because the row the slit is drawn on comes from the buffer.  A
plain newline hung on the end of LINE ends LINE's own row, which leaves LINE
that row and the line number drawn on it; LINE's own newline then ends one row
more, and that row is the slit, as tall as the newline ending it is drawn."
  (let* ((end (align-diff--line-end buffer line))
         (newline (< end (align-diff--end-of-text buffer)))
         (overlay (make-overlay end end buffer)))
    (align-diff--mark-gap-overlay overlay)
    (overlay-put overlay 'after-string
                 (if newline
                     (propertize "\n")
                   ;; No newline of its own to draw the slit on, this being
                   ;; the last line, so the string carries the colour itself.
                   (propertize "\n" 'face face)))
    (when newline
      (let ((slit (make-overlay end (1+ end) buffer)))
        (align-diff--mark-gap-overlay slit)
        (overlay-put slit 'face face)
        ;; Over the colour of the line it belongs to, since the row it draws
        ;; is the gap's and not that line's.
        (overlay-put slit 'priority 5)
        (overlay-put slit 'evaporate t)))
    overlay))

(defun align-diff--fringe-mark (buffer line face)
  "Put a triangle in BUFFER's fringe, its tip on the boundary above LINE.
Half of it under the line above and half over the line below, so the tip falls
between the two rows rather than beside either of them.  A fringe bitmap takes
no room in the text, so both lines keep their rows and their places."
  (let ((above (1- line)))
    (when (> above 0)
      ;; At the end of the line above, so a line that wraps carries the half
      ;; on its last row, which is the one against the boundary.
      (align-diff--fringe-half buffer (align-diff--line-end buffer above)
                               'align-diff-gap-upper face))
    (when (<= line (align-diff--line-count buffer))
      (align-diff--fringe-half buffer (align-buffer-line-position buffer line)
                               'align-diff-gap-lower face))))

(defun align-diff--fringe-half (buffer position bitmap face)
  "Draw BITMAP in BUFFER's fringe beside POSITION, in FACE's colour."
  (let ((overlay (make-overlay position position buffer)))
    (align-diff--mark-gap-overlay overlay)
    (overlay-put overlay 'before-string
                 (propertize "!" 'display
                             (list align-diff-gap-fringe bitmap face)))
    overlay))

(defun align-diff--clear (buffer)
  "Take align-diff's overlays out of BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (align-buffer-pad-clear buffer)
      (save-restriction
        (widen)
        (remove-overlays (point-min) (point-max) 'align-diff t)))))

(defun align-diff--line-marker (buffer line)
  "Return a marker where LINE begins in BUFFER.
A marker rather than a position, since the refinement makes one of whatever
number it is handed, in whichever buffer happens to be current."
  (with-current-buffer buffer
    (copy-marker (align-buffer-line-position buffer line))))

(defun align-diff--refine-hunk (session hunk)
  "Mark which characters of SESSION's two sides differ inside HUNK.
The hunk is compared whole, so a word moved to another line of it still
corresponds with where it came from."
  (pcase-let ((`(,old-first ,old-count ,new-first ,new-count) hunk))
    ;; A hunk with lines on one side only has nothing to correspond with, and
    ;; its lines already carry the colour that says so.
    (when (and (> old-count 0) (> new-count 0))
      (let* ((old (align-buffer-buffer session 'left))
             (new (align-buffer-buffer session 'right))
             (old-beginning (align-diff--line-marker old old-first))
             (new-beginning (align-diff--line-marker new new-first)))
        (unwind-protect
            ;; Demoted, because refining is a nicety: `smerge-refine-regions'
            ;; runs `diff-command' and reads its output back, and neither is
            ;; ours.
            (with-demoted-errors "align-diff: refining a hunk failed: %S"
              (smerge-refine-regions
               old-beginning
               (align-diff--after-line old (+ old-first old-count -1))
               new-beginning
               (align-diff--after-line new (+ new-first new-count -1))
               nil nil
               '((face . align-diff-refine-removed)
                 (align-diff . t) (align-diff-refinement . t) (priority . 10))
               '((face . align-diff-refine-added)
                 (align-diff . t) (align-diff-refinement . t) (priority . 10))))
          (set-marker old-beginning nil)
          (set-marker new-beginning nil))))))

(defun align-diff--refine (session hunks)
  "Mark which characters differ inside each of SESSION's HUNKS."
  ;; `smerge-refine-regions' walks each buffer with that buffer's own point and
  ;; leaves it where it finished, so both are put back once the pass is over.
  (let ((points (mapcar (lambda (side)
                          (with-current-buffer (align-buffer-buffer session side)
                            (point-marker)))
                        '(left right))))
    (unwind-protect
        (dolist (hunk hunks)
          (align-diff--refine-hunk session hunk))
      (dolist (marker points)
        (with-current-buffer (marker-buffer marker)
          (goto-char marker))
        (set-marker marker nil)))))

(defun align-diff--paint (session hunks)
  "Colour the lines HUNKS cover in SESSION's two buffers."
  (let ((old (align-buffer-buffer session 'left))
        (new (align-buffer-buffer session 'right)))
    (align-diff--clear old)
    (align-diff--clear new)
    (dolist (hunk hunks)
      (pcase-let ((`(,old-first ,old-count ,new-first ,new-count) hunk))
        (align-diff--mark-run old old-first old-count 'align-diff-removed)
        (align-diff--mark-run new new-first new-count 'align-diff-added)
        (when align-diff-gap-marker
          ;; Below the lines that did pair up, which is where the lines with no
          ;; counterpart begin, and named by what the pane lacks rather than by
          ;; which pane it is: the old pane's gap stands for lines the new side
          ;; gained, and the new pane's for lines it no longer has.
          ;;
          ;; A mark goes on a one-sided hunk alone, since a hunk with lines on
          ;; both sides is coloured on both and says so already.  A pad goes
          ;; wherever lines are missing, the leftover of an uneven hunk
          ;; included: it stands in for those lines, and standing in for every
          ;; one of them is what leaves the two panes reading row for row.
          (let ((gap (- old-count new-count))
                (one-sided (or (zerop old-count) (zerop new-count))))
            (when (or one-sided (eq align-diff-gap-marker 'pad))
              (cond
               ((> gap 0)
                (align-diff--mark-gap new (+ new-first new-count) 'removed
                                      gap old (+ old-first new-count)))
               ((< gap 0)
                (align-diff--mark-gap old (+ old-first old-count) 'added
                                      (- gap) new (+ new-first old-count)))))))))
    (when align-diff-refine-hunks
      (align-diff--refine session hunks))))


;;; What a session of ours carries

(defun align-diff--property (session key)
  "Return what SESSION carries under KEY."
  (plist-get (align-buffer-session-properties session) key))

(defun align-diff--set-property (session key value)
  "Have SESSION carry VALUE under KEY."
  (setf (align-buffer-session-properties session)
        (plist-put (align-buffer-session-properties session) key value)))

(defun align-diff-session-p (session)
  "Return non-nil when SESSION is one align-diff set up."
  (and (align-buffer-session-p session)
       (align-diff--property session :align-diff)
       t))

(defun align-diff--ticks (session)
  "Return SESSION's two buffers' text counters, as a cons."
  (cons (buffer-chars-modified-tick (align-buffer-buffer session 'left))
        (buffer-chars-modified-tick (align-buffer-buffer session 'right))))


;;; Keeping up with the reader's edits

(defun align-diff-refresh (&optional session)
  "Diff SESSION's buffers again, and colour them afresh.
SESSION defaults to the one of the current buffer.  The windows are brought
level again only when the pairing itself changed, so an edit that leaves the
lines corresponding as they were colours them where they stand."
  (interactive)
  (let ((session (or session (align-buffer-current-session))))
    (unless (align-diff-session-p session)
      (user-error "Not in a session align-diff set up"))
    (let* ((old (align-buffer-buffer session 'left))
           (new (align-buffer-buffer session 'right))
           (hunks (align-diff--hunks old new))
           (pairs (align-diff--pairs hunks old new)))
      (align-diff--set-property session :align-diff-ticks
                                (align-diff--ticks session))
      (align-diff--set-property session :align-diff-hunks hunks)
      (unless (equal pairs (align-diff--property session :align-diff-pairs))
        (align-diff--set-property session :align-diff-pairs pairs)
        (align-buffer-set-pairs session pairs))
      (align-diff--paint session hunks)
      session)))

(defun align-diff--idle (session)
  "Diff SESSION again when its text has changed since we last looked."
  (cond
   ((not (align-diff-session-p session))
    (align-diff--unwatch session))
   ((not (align-buffer-session-live-p session))
    (align-diff--unwatch session))
   ((not (equal (align-diff--ticks session)
                (align-diff--property session :align-diff-ticks)))
    (align-diff-refresh session))))

(defun align-diff--watch (session)
  "Have SESSION diffed again whenever its buffers have been left alone.
One timer reading two counters, rather than a hook on every change: a diff is
worth running once the typing has stopped, and that is what idleness says."
  (when align-diff-idle-delay
    (align-diff--set-property
     session :align-diff-timer
     (run-with-idle-timer align-diff-idle-delay t #'align-diff--idle session))))

(defun align-diff--unwatch (session)
  "Stop watching SESSION for edits."
  (when-let ((timer (align-diff--property session :align-diff-timer)))
    (cancel-timer timer)
    (align-diff--set-property session :align-diff-timer nil)))

(defun align-diff--teardown (session)
  "Take align-diff's timer, keys and overlays off SESSION."
  (when (align-diff-session-p session)
    (align-diff--unwatch session)
    (dolist (side '(left right))
      (let ((buffer (align-buffer-buffer session side)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (align-buffer-navigation-mode -1)))
        (align-diff--clear buffer)))))

(add-hook 'align-buffer-pre-teardown-functions #'align-diff--teardown)


;;; Showing them

;;;###autoload
(defun align-diff-buffers (old new &optional properties)
  "Show buffers OLD and NEW side by side, aligned by their differences.
PROPERTIES are carried on the session.  Return the session."
  (interactive
   (let ((new (read-buffer "Diff new buffer: " (current-buffer) t)))
     (list (read-buffer "Diff old buffer: "
                        (other-buffer (get-buffer new) t) t)
           new)))
  (let ((old (get-buffer old))
        (new (get-buffer new)))
    (unless (and (buffer-live-p old) (buffer-live-p new))
      (error "Both sides must be live buffers"))
    (let* ((hunks (align-diff--hunks old new))
           (pairs (align-diff--pairs hunks old new))
           (session (align-buffer-show
                     old new pairs
                     (append (list :align-diff t) properties))))
      (align-diff--set-property session :align-diff-hunks hunks)
      (align-diff--set-property session :align-diff-pairs pairs)
      (align-diff--set-property session :align-diff-ticks
                                (align-diff--ticks session))
      (align-diff--paint session hunks)
      (align-diff--watch session)
      (when align-diff-navigation
        (dolist (side '(left right))
          (with-current-buffer (align-buffer-buffer session side)
            (align-buffer-navigation-mode 1))))
      session)))

;;;###autoload
(defun align-diff-files (old new)
  "Show files OLD and NEW side by side, aligned by their differences.
A file already in a buffer is read from there.  One opened for this is killed
with the session."
  (interactive "fOld file: \nfNew file: ")
  (let (opened)
    (cl-flet ((buffer-for (file)
                (or (find-buffer-visiting file)
                    (let ((buffer (find-file-noselect file)))
                      (push buffer opened)
                      buffer))))
      (let ((old-buffer (buffer-for old))
            (new-buffer (buffer-for new)))
        (align-diff-buffers old-buffer new-buffer
                            (list :owned-buffers opened))))))

(provide 'align-diff)

;;; align-diff.el ends here
