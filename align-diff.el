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
          own, so walking and scrolling through them is done by
          `align-diff-next-line' and the align-buffer scroll commands.
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




(defvar-local align-diff--walked nil
  "The pad being walked and the row the cursor is on, as (OVERLAY . ROW).")

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

(defun align-diff--pad-row (face)
  "Return one blank row of a pad, faced FACE.
A space before the newline, so the row has a glyph of its own: a row drawn
from nothing at all can carry neither the cursor nor anything else, and
walking a pad is done by asking for the cursor on one of its rows."
  (propertize " \n" 'face face))

(defun align-diff--pad-face (kind)
  "Return the face a pad standing for KIND's missing lines is drawn in.

The colour of the pane the pad sits in, not of the lines it stands for, so
that the old pane reads as a loss all the way down and the new pane as a
gain, exactly as the coloured lines of the text do.  A pane's gap is named by
what that pane lacks, which puts the two names the other way round."
  (if (eq kind 'added) 'align-diff-removed 'align-diff-added))

(defun align-diff--pad-string (rows face opening)
  "Return the display of a pad of ROWS rows, faced FACE.

It opens with a newline faced OPENING, which ends the row of the line it
hangs after.  That is the face of the line itself: what paints a row to the
edge of the window is the face of whatever newline closes it, so a coloured
line needs its own colour here to be painted as far as it is anywhere else,
and an uncoloured one needs none and is left alone.

Each row after that closes with a coloured newline of its own.  The last
closes with the line's own newline instead, which the pad is drawn over so
that it is painted too."
  (concat (if opening (propertize "\n" 'face opening) "\n")
          (apply #'concat (make-list (1- rows) (align-diff--pad-row face)))
          (propertize " " 'face face)))

(defun align-diff--pad-anchor (overlay)
  "Return where the line OVERLAY's rows are drawn in front of begins.
What the rows are measured from: they are the display of the lines the other
side has, and they end where this line's own text begins."
  (overlay-end overlay))

(defun align-diff--pad-hangs (overlay)
  "Return where point belongs while OVERLAY's rows are being read.
The place the rows are drawn at, which is the end of the line they hang
after: a row carries the cursor only while point is at that place, so this is
where point waits for as long as a row is being read, and the anchor is no
good for it."
  (overlay-start overlay))

(defun align-diff--pad (buffer line rows kind &optional facing facing-line)
  "Give BUFFER ROWS blank rows above LINE, standing for KIND's missing lines.

The rows hang at the end of the line before LINE, drawn in front of that
line's own newline, which is where the lines they stand for belong and what
makes them rows of that line's display.  So every line of the text keeps its
own line number beside its own text, and the rows carry none.  At the very
beginning of the buffer there is no line before to hang them on, and they go
in front of the first line instead.

FACING is the buffer that has those lines and FACING-LINE the first of them,
which is what lets a walk of the pad scroll the facing window alongside."
  (let ((face (align-diff--pad-face kind)))
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (save-excursion
          (goto-char (if (> line (align-diff--line-count buffer))
                         (align-diff--end-of-text buffer)
                       (align-buffer-line-position buffer line)))
          (let* ((anchor (point))
                 (hangs (if (bobp) anchor
                          (save-excursion (forward-line -1)
                                          (line-end-position))))
                 ;; The colour of the line the rows hang after, read before
                 ;; the pad is there to be read instead of it.  A hunk with
                 ;; lines on both sides has its own faced already, and the
                 ;; rows of a pad hanging off one of them must not swallow
                 ;; the colour of its row.
                 (opening (and (/= hangs anchor)
                               (get-char-property hangs 'face)))
                 (overlay (make-overlay hangs anchor buffer)))
            (align-diff--mark-gap-overlay overlay)
            (overlay-put overlay 'align-diff-pad-rows rows)
            (overlay-put overlay 'align-diff-pad-facing facing)
            (overlay-put overlay 'align-diff-pad-facing-line facing-line)
            (overlay-put overlay 'align-diff-pad-opening opening)
            (unless (= hangs anchor)
              ;; The newline the rows are drawn in front of closes the last of
              ;; them, so it is painted like the others, and above the colour
              ;; of the line it belongs to.
              (overlay-put overlay 'face face)
              (overlay-put overlay 'priority 5))
            (overlay-put overlay 'before-string
                         (if (= hangs anchor)
                             (apply #'concat
                                    (make-list rows
                                               (align-diff--pad-row face)))
                           (align-diff--pad-string rows face opening)))
            overlay))))))

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
      (setq align-diff--walked nil)
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
    ;; Carried on the session, which is what align-buffer reads to know that
    ;; a placement of these two panes has to land on the pixel.
    (align-diff--set-property session :exact-alignment
                              (eq align-diff-gap-marker 'pad))
    (when align-diff-refine-hunks
      (align-diff--refine session hunks))))

(defun align-diff--exact-p (session)
  "Return non-nil when SESSION has to be levelled to the pixel.
A pad's rows fall between the row boundaries a placement lands on, so a
session with pads in it cannot read level without a lift whatever
`align-buffer-exact-alignment' says."
  (and session (align-buffer--exact-p session)))


;;; Walking a pad

;; A pad's rows are display and hold no position, so point cannot stand on
;; one.  What stands on one is the cursor: a row carrying a `cursor' property
;; is where redisplay draws it while point is at the position the pad hangs
;; on.  Walking a pad is therefore moving that property from row to row, with
;; point waiting at the line below, and scrolling the window when the row
;; being walked to would be off screen.

(defun align-diff--pad-at (position &optional buffer)
  "Return the pad whose rows are drawn in front of POSITION of BUFFER.
BUFFER goes with POSITION, the current buffer by default: a position of the
facing side read in this side's buffer finds whatever happens to lie at that
number of characters, which is a pad of the wrong pane or none.

A pad hangs off the newline before POSITION, so it is looked for over that
newline rather than at POSITION itself."
  (with-current-buffer (or buffer (current-buffer))
    (seq-find (lambda (overlay)
                (and (overlay-get overlay 'align-diff-pad-rows)
                     (= (align-diff--pad-anchor overlay) position)))
              (overlays-in (max (point-min) (1- position)) position))))

(defun align-diff--pad-rows (overlay)
  "Return how many rows OVERLAY's pad has."
  (or (overlay-get overlay 'align-diff-pad-rows) 0))

(defun align-diff--pad-show-cursor (overlay row)
  "Ask for the cursor on ROW of OVERLAY's pad, or on none when ROW is nil."
  (let* ((rows (align-diff--pad-rows overlay))
         ;; A pad in front of the first line of the buffer has no newline of
         ;; the line before to hang after, so its rows are drawn without the
         ;; one that opens the others, and every index into it is one lower.
         (hanging (/= (overlay-start overlay)
                      (align-diff--pad-anchor overlay)))
         (first (if hanging 1 0))
         (face (get-text-property first 'face
                                  (overlay-get overlay 'before-string)))
         (string (if hanging
                     (align-diff--pad-string
                      rows face (overlay-get overlay
                                             'align-diff-pad-opening))
                   (apply #'concat
                          (make-list rows (align-diff--pad-row face))))))
    (when row
      ;; The space of that row, which is the glyph the row is drawn from.
      (let ((at (+ first (* row 2))))
        (put-text-property at (1+ at) 'cursor t string)))
    (overlay-put overlay 'before-string string)))

(defun align-diff--offset-in (window position)
  "Return how far below WINDOW's top POSITION is drawn, negative when above it.
A span is measured whichever way round it is given, so a position above the
start has to be measured back to the start and turned about; the window's lift
belongs to the start's own line and so is added to that span rather than taken
off it.

What hangs at POSITION is drawn above POSITION's own text and so lies outside
the span back to the start, which counts it all the same; measuring down to
POSITION counts it on the other edge, where it belongs.  So turning the span
about puts it back, and a line drawn as eighty rows is not reported eighty
rows further off than it is."
  (let ((start (window-start window)))
    (if (>= position start)
        (cdr (window-text-pixel-size window start position))
      (+ (align-buffer--hanging-height window position)
         (- (cdr (window-text-pixel-size window start start))
            (+ (cdr (window-text-pixel-size window position start))
               (window-vscroll window t)))))))

(defun align-diff--pad-top (window overlay)
  "Return how far below WINDOW's top OVERLAY's first pad row is drawn."
  (- (align-diff--offset-in window (align-diff--pad-anchor overlay))
     (* (align-diff--pad-rows overlay) (align-buffer--row-height window))))

(defun align-diff--pad-reveal (window overlay row)
  "Scroll WINDOW so that ROW of OVERLAY's pad is clear of its edges."
  (let* ((height (align-buffer--row-height window))
         (band (align-buffer--band window))
         (top (+ (align-diff--pad-top window overlay) (* row height))))
    (cond ((< top (car band))
           (align-buffer-scroll window (- top (car band))))
          ((> (+ top height) (cdr band))
           (align-buffer-scroll window (- (+ top height) (cdr band)))))))

(defun align-diff--walk-into (overlay row &optional window reveal)
  "Put the cursor on ROW of OVERLAY's pad and keep both panes showing it.
With REVEAL, WINDOW is scrolled until that row is clear of its edges, which is
what a step of a line asks for; without it the window stays where it is, which
is what a scroll of the reader's asks for."
  (let ((window (or window (selected-window))))
    (align-diff--pad-show-cursor overlay row)
    (setq align-diff--walked (and row (cons overlay row)))
    ;; Both windows are ours while a pad is walked: the pairs say nothing
    ;; about a row that stands for a line this side has not got, so the sync
    ;; would pull them back to the pair below the pad.
    (setq align-buffer-inhibit-sync (and row t))
    ;; Point stays where the rows are drawn, which a row needs it to do to
    ;; carry the cursor at all, and that place is the end of the line they
    ;; hang after.  It is not where the step was going: the rows stand for
    ;; lines of the other side, and none of them is a position here.
    (when row
      (goto-char (align-diff--pad-hangs overlay))
      (when (eq (window-buffer window) (current-buffer))
        (set-window-point window (point))))
    ;; The line point waits on carries every one of the rows, so the cursor
    ;; walks away from point down the window while point stays where it is,
    ;; and a view that follows the cursor leaves point in the margin
    ;; redisplay scrolls for, or off the screen altogether.
    (if row
        (align-buffer-hold-margin (current-buffer))
      (align-buffer-release-margin (current-buffer)))
    (when row
      (when reveal (align-diff--pad-reveal window overlay row))
      (align-diff--pad-face-row window overlay row))))

(defun align-diff--pad-face-row (window overlay row)
  "Scroll the facing window so its own line faces ROW of OVERLAY's pad.
The pad stands for lines the facing side has, so walking its rows walks those
lines, and the two panes go on reading level."
  (let* ((facing (overlay-get overlay 'align-diff-pad-facing))
         (first (overlay-get overlay 'align-diff-pad-facing-line))
         (other (and (buffer-live-p facing) (get-buffer-window facing))))
    (when (and other first)
      (let* ((height (align-buffer--row-height window))
             (wanted (+ (align-diff--pad-top window overlay) (* row height)))
             (position (align-buffer-line-position facing (+ first row))))
        ;; Scrolled by what it is out by, again and again until it is out by
        ;; nothing: one measurement across a long stretch of a buffer can be
        ;; wide of the mark, and a pass that lands on the wrong row leaves the
        ;; next one a short span to measure, where it is not.  Bounded, since
        ;; a row the window cannot put there at all would otherwise be chased
        ;; for ever.
        (let ((tries 0)
              (out (- (align-diff--offset-in other position) wanted)))
          (while (and (not (zerop out)) (< tries 4))
            (align-buffer-scroll other out)
            (setq tries (1+ tries))
            (setq out (- (align-diff--offset-in other position) wanted))))
        ;; And that line is where the facing pane is reading: a window is
        ;; scrolled by redisplay to show its own point, so a point left behind
        ;; on a line the lift has pushed off screen pulls the pane back.
        (set-window-point other position)
        ;; Noted as ours, or the next scroll hook of that pane reads our own
        ;; scroll as a reader's: it would take the lead the moment the walk
        ;; ends and place the reader's pane from where we left it.
        (align-buffer--note-placed other (window-start other)
                                   (window-point other))))))

(defun align-diff--walk-away ()
  "Give the cursor back to the text, leaving no pad walked.

Both of the session's buffers, not merely this one: the walk is remembered
where the pad is, and a command run in the facing pane - `other-window', say -
must end it just the same, or the windows are left held for ever."
  (let* ((session (align-buffer-current-session))
         (buffers (if session
                      (list (align-buffer-buffer session 'left)
                            (align-buffer-buffer session 'right))
                    (list (current-buffer)))))
    (dolist (buffer buffers)
      (when (buffer-live-p buffer)
        (align-buffer-release-margin buffer)
        (with-current-buffer buffer
          (when align-diff--walked
            (let ((overlay (car align-diff--walked)))
              (when (overlay-buffer overlay)
                (align-diff--pad-show-cursor overlay nil)))
            (setq align-diff--walked nil))))))
  ;; The pairs can have the windows back.
  (setq align-buffer-inhibit-sync nil))

(defcustom align-diff-navigation t
  "When non-nil, a session's buffers get the commands that walk a pad.
They are what makes the rows of a pad behave like the lines they stand for:
the ordinary motion and scroll commands step over them in one go, since no
position in the buffer falls inside them."
  :type 'boolean
  :group 'align-diff)

(defcustom align-diff-wheel-rows 3
  "How many screen rows one notch of the wheel scrolls in a session."
  :type 'integer
  :group 'align-diff)

(defvar align-diff--scrolled nil
  "The window a scroll of ours has just moved, or nil.
Set where the scroll is carried out and read once by the walk check, which is
how a scroll of the view is told apart from a command that moved point without
either of them having to be named.")

(defun align-diff--event-window (event)
  "Return the window EVENT points at, or the selected one when it points nowhere.
An event bound to a key carries no position, and a wheel one sent as plain
input is such an event."
  (or (and (consp event)
           (let ((window (posn-window (event-start event))))
             (and (windowp window) window)))
      (selected-window)))

(defun align-diff-wheel-up (&optional event)
  "Scroll the window EVENT points at nearer the end of the buffer."
  (interactive (list last-input-event))
  (let ((window (align-diff--event-window event)))
    (setq align-diff--scrolled window)
    (align-buffer-scroll-rows align-diff-wheel-rows window)
    (align-diff--settle-scroll window)))

(defun align-diff-wheel-down (&optional event)
  "Scroll the window EVENT points at nearer the beginning of the buffer."
  (interactive (list last-input-event))
  (let ((window (align-diff--event-window event)))
    (setq align-diff--scrolled window)
    (align-buffer-scroll-rows (- align-diff-wheel-rows) window)
    (align-diff--settle-scroll window)))

(defvar align-diff-navigation-map
  (let ((map (make-sparse-keymap)))
    (define-key map [wheel-down] #'align-diff-wheel-up)
    (define-key map [wheel-up] #'align-diff-wheel-down)
    map)
  "What a session's buffers use for the events no command of Emacs's carries.")

(defun align-diff--pad-showing (window pixels)
  "Return the pad whose rows WINDOW draws PIXELS below its top, or nil."
  (with-current-buffer (window-buffer window)
    (let* ((position (align-buffer--line-below window pixels))
           (pad (and position (align-diff--pad-at position))))
      ;; A pad's rows come before the line's own text, so PIXELS falls inside
      ;; them exactly where that text is drawn further down than PIXELS.
      (and pad (> (align-diff--offset-in window position) pixels) pad))))

(define-minor-mode align-diff-navigation-mode
  "Minor mode moving through a pad's rows as though they were lines."
  :lighter nil
  :keymap align-diff-navigation-map
  :group 'align-diff
  (if align-diff-navigation-mode
      (progn
        ;; Before align-buffer's own hook: the walk decides which of the two
        ;; panes are ours, and the sync has to read that decision rather than
        ;; place them first and be corrected.
        (add-hook 'post-command-hook #'align-diff--walk-check nil t)
        ;; Where it beats the other minor modes rather than taking its turn
        ;; among them: `pixel-scroll-precision-mode' binds the wheel too, and
        ;; its notches scroll a window by pixels of its own without a word to
        ;; the pane facing it.
        (setq-local minor-mode-overriding-map-alist
                    (cons (cons 'align-diff-navigation-mode
                                align-diff-navigation-map)
                          (assq-delete-all 'align-diff-navigation-mode
                                           minor-mode-overriding-map-alist))))
    (align-diff--walk-away)
    (remove-hook 'post-command-hook #'align-diff--walk-check t)
    (setq-local minor-mode-overriding-map-alist
                (assq-delete-all 'align-diff-navigation-mode
                                 minor-mode-overriding-map-alist))))

(defun align-diff--step-down (original rest)
  "Take one step down, through a pad's rows where one is on the way.
Return non-nil when the step was taken here, and nil when ORIGINAL with REST
has to take it."
  (pcase align-diff--walked
    (`(,overlay . ,row)
     (if (< (1+ row) (align-diff--pad-rows overlay))
         (align-diff--walk-into overlay (1+ row) nil t)
       ;; Out at the bottom of the rows, onto the line they are drawn in front
       ;; of.  Point is waiting on the line they hang after, so the step down
       ;; is the one the reader asked for and the column is kept by it.
       (align-diff--walk-away)
       (apply original 1 rest))
     t)
    (_
     (let* ((next (save-excursion (forward-line 1) (point)))
            (pad (align-diff--pad-at next)))
       (cond
        ((and pad (> (align-diff--pad-rows pad) 0))
         (align-diff--walk-into pad 0 nil t)
         t)
        (t (apply original 1 rest) nil))))))

(defun align-diff--step-up (original rest)
  "Take one step up, through a pad's rows where one is on the way."
  (pcase align-diff--walked
    (`(,overlay . ,row)
     (if (> row 0)
         (align-diff--walk-into overlay (1- row) nil t)
       ;; Out at the top of the rows, onto the text of the line they hang
       ;; after.  Onto its beginning rather than by a step of `line-move's,
       ;; which counts the rows as a line of their own where they are drawn
       ;; and not at all where they are not.  The step has to be taken all
       ;; the same: the place point waits is the rows, and a reader left
       ;; standing there spends the press after this one getting off them.
       (align-diff--walk-away)
       (goto-char (align-diff--pad-hangs overlay))
       (beginning-of-line))
     t)
    (_
     (let ((pad (align-diff--pad-at (line-beginning-position))))
       (cond
        ((and pad (> (align-diff--pad-rows pad) 0))
         (align-diff--walk-into pad (1- (align-diff--pad-rows pad)) nil t)
         t)
        (t (apply original -1 rest) nil))))))

(defun align-diff--line-move (original count &rest rest)
  "Move COUNT lines, walking the rows of the pads on the way.

Caught here rather than under the names of the commands that move a line:
`next-line', `previous-line' and every third-party motion built on them come
through `line-move', so one interception covers them all.  A buffer with no
session of ours moves as it always does."
  (if (or (not align-diff-navigation-mode) (zerop count))
      (apply original count rest)
    (dotimes (_ (abs count))
      (if (> count 0)
          (align-diff--step-down original rest)
        (align-diff--step-up original rest)))
    nil))

(advice-add 'line-move :around #'align-diff--line-move)

(defun align-diff--scroll-rows (arg window)
  "Return how many rows ARG asks WINDOW to be scrolled by.
A windowful less the context lines where ARG says nothing, which is what the
page commands mean by no argument."
  (let ((page (max 1 (- (window-body-height window)
                        next-screen-context-lines))))
    (cond ((null arg) page)
          ((eq arg '-) (- page))
          (t (prefix-numeric-value arg)))))

(defun align-diff--scroll (original arg down)
  "Scroll ARG rows through what the lines are drawn as, DOWN for nearer the end.
Caught at `scroll-up' and `scroll-down' for the reason a line move is caught
at `line-move': every scroll command of Emacs's and of a package's comes
through one of the two, so the rows a pad adds are scrolled through rather
than jumped over whichever command the reader has."
  (if (not align-diff-navigation-mode)
      (funcall original arg)
    (let* ((window (selected-window))
           (start (window-start window))
           (lift (window-vscroll window t)))
      (setq align-diff--scrolled window)
      (align-buffer-scroll-rows
       (let ((rows (align-diff--scroll-rows arg window)))
         (if down rows (- rows)))
       window)
      (align-diff--settle-scroll window)
      ;; Signalled where the view did not move at all, as the commands we
      ;; stand in for do: a reader at the end of the buffer is told so, and
      ;; the commands built on these two have the end of their own work to
      ;; do - `evil-scroll-down' moves point the rest of the way itself.  A
      ;; scroll with somewhere to go, if not as far as it was asked, goes
      ;; there and says nothing.
      (when (and (= (window-start window) start)
                 (= (window-vscroll window t) lift))
        (signal (if down 'end-of-buffer 'beginning-of-buffer) nil))
      nil)))

(defun align-diff--scroll-up (original &optional arg)
  "Scroll nearer the end of the buffer, through what the lines are drawn as."
  (align-diff--scroll original arg t))

(defun align-diff--scroll-down (original &optional arg)
  "Scroll nearer the beginning of the buffer, through what the lines are drawn
as."
  (align-diff--scroll original arg nil))

(defun align-diff--scroll-other (original arg down)
  "Page the pair where the other window is the facing pane of a session.

The two panes of a session are one view, so there is no second window to
scroll on its own: asking for one pages both, with the pane the reader is in
leading as it does for every other scroll.  Any other window is scrolled as
it always was."
  (let ((other (and align-diff-navigation-mode
                    ;; Errors demoted because it signals where there is no
                    ;; other window at all, which is a frame holding one pane
                    ;; and nothing of ours to scroll.
                    (ignore-errors (other-window-for-scrolling)))))
    (if (not (and (window-live-p other)
                  (buffer-local-value 'align-diff-navigation-mode
                                      (window-buffer other))))
        (funcall original arg)
      (align-diff--scroll original arg down))))

(defun align-diff--scroll-other-up (original &optional arg)
  "Scroll the other window nearer the end of its buffer."
  (align-diff--scroll-other original arg t))

(defun align-diff--scroll-other-down (original &optional arg)
  "Scroll the other window nearer the beginning of its buffer."
  (align-diff--scroll-other original arg nil))

(advice-add 'scroll-other-window :around #'align-diff--scroll-other-up)
(advice-add 'scroll-other-window-down :around #'align-diff--scroll-other-down)
(advice-add 'scroll-up :around #'align-diff--scroll-up)
(advice-add 'scroll-down :around #'align-diff--scroll-down)

(defun align-diff--click-row (window event)
  "Return the row of a pad in WINDOW that EVENT was sent at, as (PAD . ROW).
Nil where it landed on the text.  A click carries no position inside a pad,
every one of its rows hanging at the one place, so the row is the one drawn
where the pointer was."
  (let* ((position (event-end event))
         (y (and (windowp (posn-window position))
                 (cdr (posn-x-y position)))))
    (when y
      ;; Measured from the top of the text, which is what a click reports
      ;; from the top of the window: anything above the text is the window's
      ;; own and takes up none of what the pads are drawn on.
      (let* ((text (- y (window-tab-line-height window)
                      (window-header-line-height window)))
             (pad (and (>= text 0) (align-diff--pad-showing window text))))
        (when pad
          (cons pad (align-diff--row-at window pad text)))))))

(defun align-diff--mouse-set-point (original event &rest rest)
  "Put the cursor on the row of a pad EVENT was sent at.

Caught at `mouse-set-point' for the reason a line move is caught at
`line-move': every command that puts point where the pointer is comes through
it, so a click lands on the row the reader clicked whichever command carried
it."
  (prog1 (apply original event rest)
    (when align-diff-navigation-mode
      (let* ((window (align-diff--event-window event))
             (clicked (and (eq (window-buffer window) (current-buffer))
                           (align-diff--click-row window event))))
        (when clicked
          (goto-char (align-diff--pad-hangs (car clicked)))
          (set-window-point window (point))
          (align-diff--walk-into (car clicked) (cdr clicked) window))))))

(advice-add 'mouse-set-point :around #'align-diff--mouse-set-point)

(defun align-diff--point-for-scroll (window &optional pixels)
  "Return the line a pad whose rows WINDOW is showing hangs on, or nil.

A row carries the cursor only while point is on the line its pad hangs on, so
that line is where point belongs for as long as the reader is reading those
rows: the row being walked while it is still drawn, and otherwise the pad
drawn where point itself would have to go.

PIXELS is where the cursor is to stay, for a scroll keeping it on its screen
row; the pad to look for is then the one drawn there rather than the one
drawn where the band begins, and a row of it is where the cursor is going
whether a row was being walked before the scroll or not."
  (when (window-live-p window)
    (with-current-buffer (window-buffer window)
      (let ((walked align-diff--walked))
        (cond
         (pixels
          (let ((pad (align-diff--pad-showing window pixels)))
            (and pad (align-diff--pad-hangs pad))))
         ((and walked (align-diff--row-shown-p window (car walked)
                                               (cdr walked)))
          (align-diff--pad-hangs (car walked)))
         ((align-diff--pad-showing window (car (align-buffer--band window)))
          (align-diff--pad-hangs
           (align-diff--pad-showing window
                                    (car (align-buffer--band window))))))))))

(setq align-buffer-point-for-scroll-function #'align-diff--point-for-scroll)

(defun align-diff--cursor-pixels (window)
  "Return how far below WINDOW's top a walked row draws the cursor, or nil.
Nil where no row of a pad is being walked, which leaves the cursor to be
measured from point as it is anywhere else."
  (when (window-live-p window)
    (let ((walked (buffer-local-value 'align-diff--walked
                                      (window-buffer window))))
      (when (and walked (overlay-buffer (car walked)))
        (+ (align-diff--pad-top window (car walked))
           (* (cdr walked) (align-buffer--row-height window)))))))

(setq align-buffer-cursor-pixels-function #'align-diff--cursor-pixels)

(defun align-diff--pad-at-top (window)
  "Return the pad whose rows the top of WINDOW falls inside, or nil."
  (with-current-buffer (window-buffer window)
    (let ((pad (align-diff--pad-at (window-start window))))
      (and pad
           (< (window-vscroll window t)
              (* (align-diff--pad-rows pad) (align-buffer--row-height window)))
           pad))))

(defun align-diff--facing-pad (session side line)
  "Return the pad standing for LINE on the side facing SIDE, as (PAD . ROW).

Nil unless LINE is one of a run the other side has not got.  The pads make
the correspondence whole: a run of lines one side alone has is drawn over
there as the rows of a pad, one row for each of those lines, so a reader on
the real lines has a row to be level with all the way down the run.  The pad
hangs on the line after the run's own pair, which is how it is found."
  (let ((anchor (align-buffer-anchor session side line)))
    (when (and anchor (> line (car anchor)))
      (let* ((facing (align-buffer-buffer session
                                          (align-buffer-other-side side)))
             (position (align-buffer-line-position facing (1+ (cdr anchor))))
             (pad (and position (align-diff--pad-at position facing)))
             (row (- line (car anchor) 1)))
        (when (and pad
                   (< row (align-diff--pad-rows pad))
                   (eq (overlay-get pad 'align-diff-pad-facing)
                       (align-buffer-buffer session side))
                   (equal (+ (or (overlay-get pad 'align-diff-pad-facing-line)
                                 0)
                             row)
                          line))
          (cons pad row))))))

(defun align-diff--face-facing-row (window pad row)
  "Scroll PAD's own window so ROW faces the line point is on in WINDOW.

The mirror of levelling a walked row: there the reader is on the pad's rows
and the real lines are over the way, here the reader is on the real lines and
the rows are.  The cursor goes on the row for the same reason it does there,
a window being scrolled by redisplay to show its own point."
  (let ((other (and (overlay-buffer pad)
                    (get-buffer-window (overlay-buffer pad)))))
    (when (window-live-p other)
      (let* ((height (align-buffer--row-height other))
             (wanted (align-diff--offset-in window (window-point window)))
             (drawn (lambda ()
                      (+ (align-diff--pad-top other pad) (* row height))))
             (tries 0)
             (out (- (funcall drawn) wanted)))
        (while (and (not (zerop out)) (< tries 4))
          (align-buffer-scroll other out)
          (setq tries (1+ tries))
          (setq out (- (funcall drawn) wanted)))
        (set-window-point other (align-diff--pad-hangs pad))
        (align-diff--pad-show-cursor pad row)
        (with-current-buffer (overlay-buffer pad)
          (setq align-diff--walked (cons pad row)))
        ;; That pane now holds point on the pad's own line too, for the same
        ;; reason and with the same margin to keep out of redisplay's way.
        (align-buffer-hold-margin (overlay-buffer pad))
        (align-buffer--note-placed other (window-start other)
                                   (window-point other))))))

(defun align-diff--inside-p (window)
  "Return non-nil when point sits inside what WINDOW is showing at all.
A point above the top or further below than a windowful is one the window has
yet to be brought to, and the shortest scroll to it is not what a reader
wants: that is a placement, and the pairs make it."
  (let ((top (align-diff--offset-in window (window-point window)))
        (bottom (window-body-height window t)))
    (and (> top (- bottom)) (< top (* 2 bottom)))))

(defun align-diff--keep-point-in-band (window)
  "Scroll WINDOW so the line point is on is drawn clear of its edges.

Ours to do while the top of the window is inside a pad.  Stepping back the
scroll margin from a line just below a pad taller than the window lands inside
the pad's rows, which redisplay cannot start a window on and still show point;
what it does instead is start the window on point's own line, and then do the
same again at every press, holding the cursor on the first row.  Scrolling by
the rows the line is short of the band keeps the line where a reader expects
it and leaves redisplay nothing to correct."
  ;; Point somewhere this view cannot show is point redisplay is about to
  ;; scroll the window to, and the lift belongs to the view being left: kept
  ;; across that scroll, it draws the line point lands on above the top of
  ;; the window, where there is no cursor to be seen.  So it goes, and
  ;; redisplay scrolls a window with nothing held back.
  (unless (align-diff--inside-p window)
    (align-buffer-forget-vscroll window))
  (when (align-diff--inside-p window)
    (let* ((height (align-buffer--row-height window))
           (band (align-buffer--band window))
           (top (align-diff--offset-in window (window-point window))))
      (cond ((< top (car band))
             (align-buffer-scroll window (- top (car band))))
            ((> (+ top height) (cdr band))
             (align-buffer-scroll window (- (+ top height) (cdr band))))))))

(defun align-diff--row-at (window overlay pixels)
  "Return the row of OVERLAY's pad WINDOW draws PIXELS below its top.
The nearest row where the pad has none there, so a caller measuring against
an edge of the window always has a row to hold on to."
  (let ((height (align-buffer--row-height window)))
    (max 0 (min (1- (align-diff--pad-rows overlay))
                (floor (- pixels (align-diff--pad-top window overlay))
                       height)))))

(defun align-diff--row-in-band (window overlay row)
  "Return ROW of OVERLAY's pad, held inside the rows WINDOW shows clear.
Nil when the pad has no row there at all.

What keeps a scroll honest: the row being read stays where it is on screen
while the view moves under it, and moves only when the view has scrolled past
it.  Clear of the margins as well, since a row against an edge is one the
facing pane cannot be brought level with, its own margin being in the way."
  (let* ((height (align-buffer--row-height window))
         (band (align-buffer--band window))
         (top (align-diff--pad-top window overlay))
         (low (max 0 (ceiling (- (car band) top) height)))
         (high (min (1- (align-diff--pad-rows overlay))
                    (floor (- (cdr band) top height) height))))
    (and (<= low high) (min high (max low row)))))

(defun align-diff--row-shown-p (window overlay row)
  "Return non-nil when ROW of OVERLAY's pad is drawn inside WINDOW."
  (and (>= (align-diff--pad-hangs overlay) (window-start window))
       (let* ((height (align-buffer--row-height window))
              (top (+ (align-diff--pad-top window overlay) (* row height))))
         (and (>= top 0)
              (<= (+ top height) (window-body-height window t))))))

(defun align-diff--show-placed-cursor (session window _side)
  "Put the cursor on a pad row when a placement has left point off screen.

A window is scrolled by redisplay to show its own point, so one placed with
its point on the line a pad hangs on cannot be held deep inside that pad: the
line's text is then below everything shown.  A row of the pad can hold the
cursor, and holding it is what lets the window stay where it was put."
  (when (and (align-diff-session-p session) (window-live-p window))
    (with-current-buffer (window-buffer window)
      (let ((offset (align-diff--offset-in window (window-point window)))
            (height (window-body-height window t))
            (row-height (align-buffer--row-height window)))
        (when (or (< offset 0) (> (+ offset row-height) height))
          ;; The pad point's own line carries, first: a line hanging a pad
          ;; taller than the window has its text below everything shown, and
          ;; the pad is what is drawn where the line belongs.  Failing that
          ;; the one the top of the window is inside, which is a window placed
          ;; further into a pad than its line.
          (let ((pad (or (align-diff--pad-at (window-point window))
                         (align-diff--pad-at-top window))))
            (when pad
              (let ((row (align-diff--row-in-band window pad 0)))
                (when row
                  (set-window-point window (align-diff--pad-hangs pad))
                  (align-diff--pad-show-cursor pad row)
                  (setq align-diff--walked (cons pad row)))))))))))

(defun align-diff--finish-placement (session window side)
  "Scroll WINDOW the rest of the way to level after a placement.

A placement lands on a row boundary, and no boundary falls inside the rows a
pad hangs, so with a pad in the way it stops short - by a whole pad's height
where the counterpart sits just below one.  Scrolling through the pad's own
display covers the rest.  Nothing to do where the placement was already
level, which is every placement with no pad between the two."
  (when (and (align-diff-session-p session)
             (align-diff--exact-p session)
             (window-live-p window))
    (let* ((leading (align-buffer-other-side side))
           (leader (get-buffer-window (align-buffer-buffer session leading)))
           (target (and leader (window-live-p leader)
                        (align-buffer--target session leading leader window))))
      (when target
        (pcase-let ((`(,position . ,pixels) target))
          ;; Only where the counterpart's own line hangs the pad.  Finishing a
          ;; placement more widely than that overrides the rules it placed by,
          ;; and measurement has it holding point against the keys.
          (when (align-diff--pad-at position (window-buffer window))
            (align-diff--level-window window position pixels)))))))

(defun align-diff--level-window (window position pixels)
  "Scroll WINDOW until POSITION is drawn PIXELS below its top.
Return non-nil when it had to move.  Bounded, since a pixel the window cannot
show POSITION at would otherwise be chased for ever."
  (let ((tries 0)
        (out (- (align-diff--offset-in window position) pixels)))
    (while (and (not (zerop out)) (< tries 4))
      (align-buffer-scroll window out)
      (setq tries (1+ tries))
      (setq out (- (align-diff--offset-in window position) pixels)))
    (> tries 0)))

(defun align-diff--after-place (session window side)
  "Finish the placement of WINDOW, which shows SIDE of SESSION.

Both parts in one go, and the window noted as ours again at the end: a
placement is noted before this runs, so a start or a point we change here
leaves that note stale, and a stale note reads as a reader having moved the
window - which hands it the lead and has the two panes placing each other."
  (align-diff--finish-placement session window side)
  (align-diff--show-placed-cursor session window side)
  (when (window-live-p window)
    (align-buffer--note-placed window (window-start window)
                               (window-point window))))

(add-hook 'align-buffer-post-place-functions #'align-diff--after-place)

(defun align-diff--walk-kept-p ()
  "Return non-nil while the walk of a pad is still where the reader is.

Point stays on the line a pad hangs on for as long as its rows are walked,
which is what lets any command whatever be told apart from a step of the
walk: a point somewhere else is a command having moved it, and the walk is
over.  Told apart by what the command did rather than by its name, so the
unbounded set of commands that move point needs no list, and a command that
moves neither point nor the view - a prefix key, `execute-extended-command',
a look at the documentation - leaves the reader where they were."
  (pcase align-diff--walked
    (`(,overlay . ,_)
     (and (overlay-buffer overlay)
          (eq (current-buffer) (overlay-buffer overlay))
          (= (point) (align-diff--pad-hangs overlay))))))

(defun align-diff--settle-scroll (window)
  "Put the cursor on the row of a pad WINDOW has just been scrolled to.
Return non-nil when a row of a pad took it, and nil where the view has none to
read.

A scroll says where the view is, and the row being read follows it: it stays
where it is on screen while the view moves under it, and moves only once the
view has gone past it.  The pairs cannot say any of this, the line a pad hangs
on being below everything the window is showing.

Called as the scroll is made, and not merely once the command that made it is
over.  The rows are display and the cursor on one of them is a property of
that display, so a redisplay anywhere inside that command draws the cursor
wherever it was last asked for; a cursor outside the view is one redisplay
scrolls the window to, and the scroll it makes puts the window start inside
the pad's own overlay, where not one of its rows is drawn."
  (with-current-buffer (window-buffer window)
    (let* ((band (align-buffer--band window))
           ;; Where the cursor is to stay, for a scroll that keeps it on its
           ;; screen row, and the top of the band for one that does not: that
           ;; is where the reader is looking either way.
           (kept align-buffer-scroll-kept-pixels)
           (at (or kept (car band)))
           ;; The pad drawn there, which is not always the one the window
           ;; starts inside: a scroll can leave a line and then a pad taller
           ;; than the window, and those rows are then all there is to read.
           (pad (align-diff--pad-showing window at))
           (was (and (null kept) align-diff--walked
                     (eq (car align-diff--walked) pad)
                     (cdr align-diff--walked)))
           (row (and pad (align-diff--row-in-band
                          window pad
                          (or was (align-diff--row-at window pad at))))))
      (when row
        (align-diff--walk-into pad row window)
        t))))

(defun align-diff--walking-p ()
  "Return non-nil while the rows of a pad are being read on either side.

The rows may be the facing pane's rather than this one's: a reader standing in
a run of lines the other side has not got has that side walking the pad which
stands for them, and holds both panes while it lasts.  So a scroll with no row
left to read has as much to give back there as here."
  (or align-diff--walked align-buffer-inhibit-sync))

(defun align-diff--walk-check ()
  "Keep or end the walk of a pad, by what the command just run did.

A nil command is no command of the reader's: the hook runs again with nothing
in `this-command' between one command and the next, and a walk has to live
through that to last longer than a single press."
  (when this-command
    ;; Before the view is read, since what the scroll asked for is what the
    ;; rows are measured against: the command that scrolled may have taken
    ;; the lift off again on its way out.
    (align-buffer-keep-scrolled-view)
    (let* ((scrolled align-diff--scrolled)
           ;; The window the wheel scrolled, which is not always the selected
           ;; one: a notch over the facing pane moves that pane, and the walk
           ;; to keep or end is the one in its own buffer.
           (window (if (window-live-p scrolled) scrolled (selected-window))))
      (setq align-diff--scrolled nil)
      (with-current-buffer (window-buffer window)
        (align-diff--walk-check-1 window scrolled)))))

(defun align-diff--walk-check-1 (window scrolled)
  "Keep or end the walk of a pad in WINDOW, SCROLLED when a scroll moved it."
  (cond
   (scrolled
    ;; A scroll says where the view is, and the row being read follows it: it
    ;; stays where it is on screen while the view moves under it, and moves
    ;; only once the view has gone past it.  The pairs cannot say any of this,
    ;; the line a pad hangs on being below everything the window is showing.
    ;;
    ;; Only from a scroll: a line move that happens to leave the top of the
    ;; window inside some other pad is not a reader walking that pad.
    (cond
     ((align-diff--settle-scroll window))
     ((align-diff--walking-p)
      (align-diff--walk-away)
      ;; The row that was carrying the cursor is gone, so point has to be
      ;; somewhere the window shows again, or redisplay scrolls to it - and
      ;; where the scroll was keeping the cursor on its screen row, that is
      ;; where it goes on this press too: the row it was reading has gone by,
      ;; and a line is drawn where the row was.
      (align-buffer--keep-point-shown window align-buffer-scroll-kept-pixels)
      ;; The pairs have the windows back, so they can say what is level.
      (align-buffer-sync window))
     ((not (eq window (selected-window)))
      ;; A scroll of the pane the reader is not standing in -
      ;; `scroll-other-window' - is still the view they asked for, so that
      ;; pane leads and the one they are in comes level with it.  Nothing
      ;; else speaks for it: the hook that syncs after a command speaks for
      ;; the selected window alone.
      (align-buffer-sync window))))
   ((null align-diff--walked) nil)
   ((not (align-diff--walk-kept-p)) (align-diff--walk-away))
   ;; A row off the screen is one redisplay scrolls back to, so a walk lasts
   ;; as long as its row is drawn; the facing pane is brought level with the
   ;; row again while it is.
   ((align-diff--row-shown-p window (car align-diff--walked)
                             (cdr align-diff--walked))
    (align-diff--pad-face-row window (car align-diff--walked)
                              (cdr align-diff--walked)))
   (t
    (align-diff--walk-away)
    (align-buffer--keep-point-shown window)
    (align-buffer-sync window)))
  ;; A walked row carries the cursor itself.  Anything else leaves point where
  ;; the pairs cannot speak for it, and two things are then ours: a line
  ;; standing in a run the other side has not got, which faces a row of a pad
  ;; over there, and showing point at all above a pad, which redisplay cannot.
  (unless align-diff--walked
    ;; The reader's own pane first: a line left inside its margin is one
    ;; redisplay scrolls a row for, and the facing pane has to be brought
    ;; level with the row the line ends up on rather than the one it is
    ;; passing through, or it is left a row out every press.  A scroll has
    ;; already put point on a line drawn clear of the edges, so there is
    ;; nothing there for the band to do and a scroll of its own would only
    ;; take back part of the reader's.
    (unless scrolled (align-diff--keep-point-in-band window))
    (align-diff--face-run window)))

(defun align-diff--finish-level (window)
  "Scroll the facing pane the rest of the way to level with WINDOW.

A placement lands on a row boundary and no boundary falls inside the rows a
pad hangs, so a counterpart sitting behind a pad taller than the window is
left about a pad's height short of level.  Done from here and not only from
the hook a placement runs: a placement that finds the window already where it
means to put it moves nothing, runs no hook, and leaves it short for good."
  (let* ((session (align-buffer-current-session))
         (side align-buffer--side)
         (facing (and session side (align-buffer-other-side side)))
         (other (and facing
                     (get-buffer-window (align-buffer-buffer session facing))))
         (target (and (window-live-p other)
                      (align-buffer--target session side window other))))
    (when target
      (pcase-let ((`(,position . ,pixels) target))
        (when (align-diff--level-window other position pixels)
          (align-diff--show-placed-cursor session other facing)
          (align-buffer--note-placed other (window-start other)
                                     (window-point other)))))))

(defun align-diff--face-run (window)
  "Keep the facing pane level while point is in a run the other side has not got.
The pairs say nothing inside such a run, so the sync would hold the facing
pane at the pair below it; what belongs beside those lines is the rows of the
pad that stands for them, and they are ours to keep level."
  (let* ((session (align-buffer-current-session))
         (side align-buffer--side)
         ;; The window's own point rather than the buffer's: a scroll of the
         ;; pane the reader is not standing in is levelled from here too, and
         ;; a buffer shown in a window it is not selected in keeps a point of
         ;; its own that has nothing to do with what that window shows.
         (line (and session side
                    (align-buffer-line-at (current-buffer)
                                          (window-point window))))
         (facing (and line (align-diff--facing-pad session side line))))
    (cond
     (facing
      ;; Both panes are ours while the run lasts, for the same reason they are
      ;; while a pad is walked.
      (setq align-buffer-inhibit-sync t)
      (align-diff--face-facing-row window (car facing) (cdr facing)))
     (align-buffer-inhibit-sync
      (align-diff--walk-away)
      (align-buffer-sync window)
      (when (align-diff--exact-p session) (align-diff--finish-level window)))
     ((align-diff--exact-p session)
      (align-diff--finish-level window)))))


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
          (with-current-buffer buffer (align-diff-navigation-mode -1)))
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
            (align-diff-navigation-mode 1))))
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
