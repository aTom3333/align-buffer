;;; align-diff-core.el --- Show two buffers by a reading of their differences -*- lexical-binding: t; -*-

;; Author: Thomas Ferrand
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, vc
;; URL: https://github.com/aTom3333/align-buffer

;;; Commentary:

;; Everything align-diff does once something has worked out what corresponds.
;; What works that out is a BACKEND, and what it hands over is a READING: a
;; plist saying which lines pair with which, which of them to colour, where a
;; side has no lines for the other's, which stretches the reader moves
;; between, and which characters differ inside a changed line.
;;
;;   :pairs     ((OLD-LINE . NEW-LINE) ...), the correspondence itself.
;;   :sections  (LEFT-LINE LEFT-COUNT RIGHT-LINE RIGHT-COUNT) apiece, which is
;;              what `align-buffer-next-section' moves between.
;;   :runs      (SIDE FIRST COUNT) apiece, the lines to colour; which colour
;;              follows from the side.
;;   :gaps      (SIDE LINE KIND COUNT FACING-LINE ONE-SIDED) apiece, where
;;              SIDE has no lines for COUNT of the other's, which belong in
;;              front of LINE and begin at FACING-LINE over there.  ONE-SIDED
;;              says the stretch has lines on that one side alone, which is
;;              the stretch a mark is for: one with lines on both is coloured
;;              on both and says so already.
;;   :refine    how the characters that differ are found, either
;;              (regions (OLD-FIRST OLD-COUNT NEW-FIRST NEW-COUNT) ...) from a
;;              backend that knows the stretches and leaves the comparing to
;;              `smerge-refine-regions', or (ranges (SIDE LINE START END) ...)
;;              from one that knows the characters itself.
;;
;; A backend is a function of two buffers returning a reading, registered
;; under a name with `align-diff-register-backend'.  `align-diff-backend' says
;; which name a session is shown with, and the session carries that name, so
;; being read again after an edit goes the way it was shown.
;;
;; The colours are overlays in the buffers themselves, at two levels: the
;; lines a change covers, and the characters that differ inside it.  Nothing
;; is copied anywhere, so the text under them is the reader's own buffer with
;; its mode, its font lock and its editing intact.
;;
;; Editing is therefore allowed, and moves the difference: once both buffers
;; have been left alone for `align-diff-idle-delay', they are read again, the
;; colours are painted afresh, and the windows are brought level again if the
;; correspondence itself changed.  `align-diff-refresh' does the same on
;; demand.

;;; Code:

(require 'cl-lib)
(require 'align-buffer)
(require 'smerge-mode)

(defgroup align-diff nil
  "Two buffers side by side, aligned by their differences."
  :group 'align-buffer
  :prefix "align-diff-")

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

(defvar-local align-diff-language-extension nil
  "The extension a backend reads this buffer's language off, as a string.
Set by whoever puts the text here, and read in front of the buffer's own
names: a buffer holding a revision of a file visits no file of its own, and
what its name ends in says nothing about the language.  When nil the names are
what a backend goes by.")


;;; Which backend reads the difference

(defvar align-diff-backends nil
  "An alist of NAME to the function reading a difference under that name.
The function is called with two buffers, the old and the new, and returns a
reading.  `align-diff-register-backend' is what puts one here.")

(defun align-diff-register-backend (name function)
  "Have FUNCTION read a difference under NAME."
  (setf (alist-get name align-diff-backends) function))

(defcustom align-diff-backend 'diff
  "Which of `align-diff-backends' reads the difference for a new session.
A session carries the name it was shown with, so changing this leaves the
sessions already up as they are."
  :type 'symbol
  :group 'align-diff)

(defun align-diff--backend (name)
  "Return the function reading a difference under NAME."
  (or (alist-get name align-diff-backends)
      (error "No align-diff backend called %s" name)))

(defun align-diff--read (name old new)
  "Return backend NAME's reading of buffer OLD against buffer NEW."
  (funcall (align-diff--backend name) old new))

(defun align-diff-reading-pairs (reading)
  "Return READING's correspondence."
  (plist-get reading :pairs))

(defun align-diff-reading-sections (reading)
  "Return the stretches READING calls sections."
  (plist-get reading :sections))

(defun align-diff-reading-runs (reading)
  "Return the runs of lines READING has coloured."
  (plist-get reading :runs))

(defun align-diff-reading-gaps (reading)
  "Return where READING says a side has no lines for the other's."
  (plist-get reading :gaps))

(defun align-diff-reading-refine (reading)
  "Return how READING says the characters that differ are found."
  (plist-get reading :refine))


;;; Counting the lines

(defun align-diff--line-count (buffer)
  "Return how many lines BUFFER holds, narrowing or not."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (count-lines (point-min) (point-max)))))


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

(defun align-diff--refine-ranges (session ranges)
  "Mark the characters RANGES names in SESSION's two buffers.
Each range is (SIDE LINE START END), START and END counting characters from
the beginning of LINE.  A range of no width is left alone: there is nothing
of it to see, and an overlay over nothing is one the line's own colour shows
through."
  (dolist (range ranges)
    (pcase-let ((`(,side ,line ,start ,end) range))
      (when (> end start)
        (let* ((buffer (align-buffer-buffer session side))
               (position (align-buffer-line-position buffer line)))
          (when position
            (with-current-buffer buffer
              (save-restriction
                (widen)
                ;; Held to the line itself: a range reaching past its end is a
                ;; range read of a line that has since been edited, and an
                ;; overlay over the newline would colour the whole row.
                (let* ((last (align-diff--line-end buffer line))
                       (from (min (+ position start) last))
                       (to (min (+ position end) last)))
                  (when (> to from)
                    (let ((overlay (make-overlay from to buffer)))
                      (overlay-put overlay 'face
                                   (if (eq side 'left)
                                       'align-diff-refine-removed
                                     'align-diff-refine-added))
                      (overlay-put overlay 'align-diff t)
                      (overlay-put overlay 'align-diff-refinement t)
                      (overlay-put overlay 'priority 10))))))))))))

(defun align-diff--refine-reading (session refine)
  "Mark the characters REFINE says differ in SESSION's two buffers."
  (pcase refine
    (`(regions . ,regions) (align-diff--refine session regions))
    (`(ranges . ,ranges) (align-diff--refine-ranges session ranges))))

(defun align-diff--paint (session reading)
  "Colour SESSION's two buffers as READING says."
  (let ((old (align-buffer-buffer session 'left))
        (new (align-buffer-buffer session 'right)))
    (align-diff--clear old)
    (align-diff--clear new)
    (dolist (run (align-diff-reading-runs reading))
      (pcase-let ((`(,side ,first ,count) run))
        (align-diff--mark-run (align-buffer-buffer session side) first count
                              (if (eq side 'left)
                                  'align-diff-removed
                                'align-diff-added))))
    (when align-diff-gap-marker
      (dolist (gap (align-diff-reading-gaps reading))
        (pcase-let ((`(,side ,line ,kind ,count ,facing-line ,one-sided) gap))
          ;; A mark goes where the pane has none of the other's lines and so
          ;; nothing of its own to colour there.  A pad goes wherever lines
          ;; are missing, the leftover of an uneven stretch included: it
          ;; stands in for those lines, and standing in for every one of them
          ;; is what leaves the two panes reading row for row.
          (when (or one-sided (eq align-diff-gap-marker 'pad))
            (align-diff--mark-gap (align-buffer-buffer session side) line kind
                                  count
                                  (align-buffer-buffer
                                   session (align-buffer-other-side side))
                                  facing-line)))))
    (when align-diff-refine-hunks
      (align-diff--refine-reading session
                                  (align-diff-reading-refine reading)))))


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

(defun align-diff--apply (session reading)
  "Have SESSION show READING, and remember the text it was read from.
The windows are brought level again only where the correspondence itself
changed: a reading that pairs the lines as they were paired already leaves
them where they stand."
  (align-diff--set-property session :align-diff-ticks
                            (align-diff--ticks session))
  (align-buffer-set-sections session (align-diff-reading-sections reading))
  (let ((pairs (align-diff-reading-pairs reading)))
    (unless (equal pairs (align-diff--property session :align-diff-pairs))
      (align-diff--set-property session :align-diff-pairs pairs)
      (align-buffer-set-pairs session pairs)))
  (align-diff--paint session reading)
  session)

(defun align-diff-refresh (&optional session)
  "Read SESSION's buffers again, and colour them afresh.
SESSION defaults to the one of the current buffer, and is read by the backend
it was shown with."
  (interactive)
  (let ((session (or session (align-buffer-current-session))))
    (unless (align-diff-session-p session)
      (user-error "Not in a session align-diff set up"))
    (align-diff--apply
     session
     (align-diff--read (align-diff--session-backend session)
                       (align-buffer-buffer session 'left)
                       (align-buffer-buffer session 'right)))))

(defun align-diff--session-backend (session)
  "Return the name of the backend SESSION was shown with."
  (or (align-diff--property session :align-diff-backend) align-diff-backend))

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

(defun align-diff-show (old new backend &optional properties)
  "Show buffers OLD and NEW side by side, read by BACKEND.
PROPERTIES are carried on the session.  Return the session."
  (let ((old (get-buffer old))
        (new (get-buffer new)))
    (unless (and (buffer-live-p old) (buffer-live-p new))
      (error "Both sides must be live buffers"))
    (let* ((reading (align-diff--read backend old new))
           (session (align-buffer-show
                     old new (align-diff-reading-pairs reading)
                     (append (list :align-diff t
                                   :align-diff-backend backend)
                             properties))))
      (align-diff--set-property session :align-diff-pairs
                                (align-diff-reading-pairs reading))
      (align-diff--apply session reading)
      (align-diff--watch session)
      (when align-diff-navigation
        (dolist (side '(left right))
          (with-current-buffer (align-buffer-buffer session side)
            (align-buffer-navigation-mode 1))))
      session)))

;;;###autoload
(defun align-diff-buffers (old new &optional properties)
  "Show buffers OLD and NEW side by side, aligned by their differences.
PROPERTIES are carried on the session.  Return the session.

Read by `align-diff-backend', whose name the session carries so that reading
it again after an edit goes the same way."
  (interactive
   (let ((new (read-buffer "Diff new buffer: " (current-buffer) t)))
     (list (read-buffer "Diff old buffer: "
                        (other-buffer (get-buffer new) t) t)
           new)))
  (align-diff-show old new align-diff-backend properties))

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

(provide 'align-diff-core)

;;; align-diff-core.el ends here
