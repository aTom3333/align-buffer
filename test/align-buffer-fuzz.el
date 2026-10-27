;;; align-buffer-fuzz.el --- Random correspondences walked at random -*- lexical-binding: t; -*-

;;; Commentary:

;; A seed builds two buffers and a correspondence between them, and then a walk
;; over them: arrows, pages, wheel notches, single-row scrolls, jumps to a line,
;; moves from one marked stretch to the next, swaps of which window leads, and
;; resizes of the pane the reader is in, one press at a time and several in a
;; row the way a repeating key gives them.
;; After every step the two windows are read back and checked against what the
;; sync promises, so a seed either passes or names the step it broke on and the
;; numbers it broke with.
;;
;; A seed decides everything: the shape of the script, the lengths, the notes,
;; the margin, whether the buffers wrap, whether the runs one side alone has
;; are given rows standing for them, and the walk itself.  So a failure is
;; replayed by its seed and step with nothing else recorded.
;;
;; Four promises are checked, each of which holds no matter what the buffers
;; look like, which is what makes a random case judgeable at all:
;;
;; Level.  The governing pair is drawn on the same pixel row in both windows.
;; The follower can only push its line of the pair down as far as it has text
;; above that line, so where it has less than the leader asks for, the promise
;; is instead that it is showing its first line with nothing held back.
;;
;; Point.  `scroll-margin' keeps a band of rows clear at each edge, and the
;; follower's point goes on the pair whenever the pair is drawn inside that
;; band.  Drawn outside it, point goes on the row nearest the pair that the band
;; allows.  The two cases are reported apart: point off a pair the band allowed
;; is a fault of the sync, point off the row the band allowed is a fault of the
;; walk to the nearest row.
;;
;; Settled.  Placing the follower again moves nothing.  A placement that is not
;; a fixed point is one the next redisplay is free to undo.
;;
;; Rows for a run.  Where a seed hangs rows for the lines one side has not got,
;; the pairs say nothing inside such a run and that promise is checked in place
;; of Level: the row standing for a line and the line itself are drawn on the
;; same pixel row, whichever of the two the reader is on.
;;
;; How jumpy a step was is counted and reported but not judged: it is how many
;; different positions the follower held between the command and the screen
;; coming to rest.
;;
;; `M-x align-buffer-fuzz' walks one seed and reports in the echo area.  In
;; batch, `align-buffer-fuzz-batch' walks a range of them and writes a report.

;;; Code:

(require 'cl-lib)
(require 'align-buffer)
(require 'align-buffer-background)

(defvar align-buffer-fuzz-steps 24
  "How many steps a walk over one seed takes.")

(defvar align-buffer-fuzz-ask-again t
  "Whether to place the follower a second time and see whether it moves.
Nil leaves the walk judging only the states a reader's own keys and notches
reach, since the second placement is one nothing but the walk asks for.")

(defvar align-buffer-fuzz--state 1
  "The generator's state, so that a seed decides the whole run.")

(defvar align-buffer-fuzz--reversals 0
  "How many times a follower moved away from a position and back to it.")

(defvar align-buffer-fuzz--at-rest nil
  "The faults still there once the two panes have come to rest.")

(defvar align-buffer-fuzz--faults nil
  "What the current run found, newest first.")

(defvar align-buffer-fuzz--jumpy nil
  "How many positions the follower held per step of the current run.")

(defvar align-buffer-fuzz--shape nil
  "What the current run's seed built, for the report to name.")

(defvar align-buffer-fuzz--unreachable 0
  "Steps of the current run where no start of the follower could be level.")

(defvar align-buffer-fuzz--padded 0
  "How many steps had a pad's row to be level with instead of a pair.")

(defvar align-buffer-fuzz--unanchored 0
  "Steps of the current run that left the leader not drawing its own pair.")


;;; A generator of our own, so a seed means the same thing in every run

(defun align-buffer-fuzz--seed (seed)
  "Start the generator from SEED."
  (setq align-buffer-fuzz--state (1+ (mod (abs seed) 2147483646))))

(defun align-buffer-fuzz--random (limit)
  "Return a number below LIMIT.
The low bits of a linear congruential generator are the poor ones, so the
state is divided before it is reduced."
  (setq align-buffer-fuzz--state
        (mod (+ (* 1103515245 align-buffer-fuzz--state) 12345) 2147483648))
  (mod (/ align-buffer-fuzz--state 256) (max 1 limit)))

(defun align-buffer-fuzz--pick (choices)
  "Return one of CHOICES."
  (nth (align-buffer-fuzz--random (length choices)) choices))

(defun align-buffer-fuzz--between (low high)
  "Return a number from LOW to HIGH."
  (+ low (align-buffer-fuzz--random (1+ (- high low)))))


;;; The script: what each side's lines are, and which of them pair

(defconst align-buffer-fuzz--segment-kinds
  '(shared shared shared shared added added removed removed moved moved
           grown shrunk tall promoted)
  "What a segment of a script can be, named repeatedly to weight the draw.
A shared segment reads the same on both sides.  Added and removed sit on one
side only.  A moved segment is written on the left where it comes up and on the
right at a later boundary, which is what makes a pair run backwards.  Grown and
shrunk pair across but differ in length, so one side wraps where the other does
not.  Tall is drawn above a row's height on both sides and promoted on the
right only, so a row is not worth the same on the two sides.")

(defun align-buffer-fuzz--script ()
  "Return (LEFT RIGHT), each a list of (ID . KIND) items in the order shown.
An identity in both lists is a pair, so the correspondence follows from the
script rather than being drawn separately."
  (let ((id 0) (left nil) (right nil) (waiting nil))
    (dotimes (_ (align-buffer-fuzz--between 6 14))
      (let ((kind (align-buffer-fuzz--pick align-buffer-fuzz--segment-kinds))
            (count (align-buffer-fuzz--between 1 25)))
        (pcase kind
          ('added
           (dotimes (_ count) (push (cons (cl-incf id) 'added) right)))
          ('removed
           (dotimes (_ count) (push (cons (cl-incf id) 'removed) left)))
          ('moved
           (let ((block nil))
             (dotimes (_ (align-buffer-fuzz--between 2 20))
               (push (cons (cl-incf id) 'moved) block))
             (setq block (nreverse block))
             (dolist (item block) (push item left))
             (push block waiting)))
          (_
           (dotimes (_ count)
             (let ((item (cons (cl-incf id) kind)))
               (push item left)
               (push item right))))))
      ;; A block that moved lands on the right at a boundary of its own, so how
      ;; far it moved varies from block to block.
      (when (and waiting (zerop (align-buffer-fuzz--random 2)))
        (dolist (item (pop waiting)) (push item right))))
    (dolist (block (nreverse waiting))
      (dolist (item block) (push item right)))
    (list (nreverse left) (nreverse right))))

(defun align-buffer-fuzz--long (words)
  "Return a comment WORDS words long."
  (concat " // " (mapconcat #'identity (make-list words "and so on") " ")))

(defun align-buffer-fuzz--text (item side)
  "Return the text SIDE shows for ITEM.
A paired line reads the same on both sides, save for the two kinds that differ
in length on purpose."
  (let ((id (car item)))
    (pcase (cdr item)
      ('grown (format "\tgrown(%d);%s" id
                      (if (eq side 'right) (align-buffer-fuzz--long 25) "")))
      ('shrunk (format "\tshrunk(%d);%s" id
                       (if (eq side 'left) (align-buffer-fuzz--long 25) "")))
      ;; Every third line of a run the other side has not got is long enough
      ;; to wrap where the buffer wraps.  A pad stands one row against each of
      ;; these lines, so a line taking two rows over here is a row the pad has
      ;; to be as tall as, and everything below the run is out by one for each
      ;; such line where it is not.
      ('added (format "\tadded(%d);%s" id
                      (if (zerop (% id 3)) (align-buffer-fuzz--long 20) "")))
      ('removed (format "\tremoved(%d);%s" id
                        (if (zerop (% id 3)) (align-buffer-fuzz--long 20) "")))
      ('moved (format "\tmoved(%d);" id))
      ('tall (format "\ttall(%d);" id))
      ('promoted (format "\tpromoted(%d);" id))
      (_ (if (zerop (% id 20))
             (format "\tshared(%d);%s" id (align-buffer-fuzz--long 12))
           (format "\tshared(%d);" id))))))

(defun align-buffer-fuzz--face (item side scale)
  "Return the face SIDE draws ITEM with at SCALE, or nil for the usual one."
  (pcase (cdr item)
    ('tall (list :height scale))
    ('promoted (and (eq side 'right) (list :height scale)))))


;;; The two buffers, their notes, and the session over them

(defun align-buffer-fuzz--buffer (name side items scale wrapped numbered margin
                                       spacing)
  "Return a buffer called NAME holding SIDE's text for ITEMS.
SCALE is how much taller than a row a tall line is drawn.  WRAPPED and NUMBERED
say whether the buffer wraps its long lines and shows line numbers, MARGIN is
its `scroll-margin', and SPACING its `line-spacing', which makes a row worth
more than the font asks for."
  (let ((buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (remove-overlays)
        (dolist (item items)
          (let ((from (point))
                (face (align-buffer-fuzz--face item side scale)))
            (insert (align-buffer-fuzz--text item side) "\n")
            (when face
              (overlay-put (make-overlay from (point)) 'face face)))))
      (delay-mode-hooks (fundamental-mode))
      (display-line-numbers-mode (if numbered 1 -1))
      (visual-line-mode (if wrapped 1 -1))
      (setq-local scroll-margin margin)
      (setq-local line-spacing spacing)
      ;; A screen row at a time, which moves the window without moving point and
      ;; so drives the sync from the other end than the arrows do.
      (local-set-key (kbd "M-<down>") #'scroll-up-line)
      (local-set-key (kbd "M-<up>") #'scroll-down-line)
      ;; The section navigation carries no keys of its own, and the walk sends
      ;; real input, so it is given a pair here.
      (local-set-key (kbd "C-c n") #'align-buffer-next-section)
      (local-set-key (kbd "C-c p") #'align-buffer-previous-section)
      (local-set-key (kbd "C-c g") #'align-buffer-fuzz-goto-line)
      (local-set-key (kbd "C-c l") #'align-buffer-fuzz-recenter)
      (local-set-key (kbd "C-c w") #'align-buffer-fuzz-widen-pane)
      (local-set-key (kbd "C-c s") #'align-buffer-fuzz-narrow-pane)
      (set-buffer-modified-p nil)
      (goto-char (point-min)))
    buffer))

(defun align-buffer-fuzz--note (buffer line rows scaled)
  "Hang a note of ROWS screen rows above LINE of BUFFER.
Drawn above a row's height when SCALED, so the note's rows are not a whole
number of the window's rows.  The note is a `before-string', the way a checker
or a blame header puts one there: rows of the display that no position in the
buffer stands inside."
  (with-current-buffer buffer
    (let* ((position (align-buffer-line-position buffer line))
           (overlay (make-overlay position position))
           (face (if scaled '(font-lock-warning-face (:height 1.6))
                   'font-lock-warning-face)))
      (overlay-put overlay 'align-buffer-fuzz-note t)
      (overlay-put
       overlay 'before-string
       (mapconcat (lambda (row)
                    (propertize (format "\t>> a note above line %d, row %d of %d\n"
                                        line (1+ row) rows)
                                'face face))
                  (number-sequence 0 (1- rows))
                  "")))))

(defun align-buffer-fuzz--lines (buffer)
  "Return how many lines of text BUFFER holds."
  (with-current-buffer buffer
    (max 1 (1- (line-number-at-pos (point-max))))))

(defun align-buffer-fuzz--pairs (left right)
  "Return a pair of line numbers for each identity LEFT and RIGHT share."
  (let ((places (make-hash-table :test #'eq))
        (line 0)
        (pairs nil))
    (dolist (item left) (puthash (car item) (cl-incf line) places))
    (setq line 0)
    (dolist (item right)
      (cl-incf line)
      (when-let ((at (gethash (car item) places)))
        (push (cons at line) pairs)))
    (nreverse pairs)))

(defface align-buffer-fuzz-pad '((t :inherit font-lock-string-face :extend t))
  "What a pad's rows are drawn in, standing for a consumer's own colour.")

(defun align-buffer-fuzz--places (items)
  "Return a hash from each identity in ITEMS to the line it is on."
  (let ((places (make-hash-table :test #'eq))
        (line 0))
    (dolist (item items) (puthash (car item) (cl-incf line) places))
    places))

(defun align-buffer-fuzz--run-of (items places from)
  "Return the last line of the run of ITEMS beginning at FROM that PLACES lacks.
A maximal run: the line after it is one the facing side has, or none at all."
  (let ((last from)
        (lines (length items)))
    (while (and (< last lines)
                (null (gethash (car (nth last items)) places)))
      (setq last (1+ last)))
    last))

(defun align-buffer-fuzz--runs (left-items right-items)
  "Return what a consumer would mark in LEFT-ITEMS and RIGHT-ITEMS.

As (SECTIONS . PADS).  Each section is (LEFT-LINE LEFT-COUNT RIGHT-LINE
RIGHT-COUNT) the way align-buffer takes them, so a run one side alone has
counts none on the other and sits in front of the line the missing ones belong
before, which is the shape a diff's hunks come out in.

Each pad is (SIDE LINE ROWS FACING-LINE): rows to hang in front of LINE of
SIDE, standing for the facing side's lines from FACING-LINE on.  A run gets
one only where the facing side has nothing at all between the pairs either end
of it; a correspondence drawn at random can pair the line above a run and the
line below it with lines far apart, and rows hung between those stand for
nothing a reader could be reading."
  (let ((left-places (align-buffer-fuzz--places left-items))
        (right-places (align-buffer-fuzz--places right-items))
        (sections nil)
        (pads nil))
    (dolist (ends (list (list 'left left-items right-places 'right)
                        (list 'right right-items left-places 'left)))
      (pcase-let ((`(,side ,items ,facing-places ,facing) ends))
        (let ((line 0)
              (lines (length items)))
          (while (< line lines)
            (if (gethash (car (nth line items)) facing-places)
                (setq line (1+ line))
              (let* ((last (align-buffer-fuzz--run-of items facing-places line))
                     (first (1+ line))
                     (count (- last line))
                     ;; Where the facing side stands either side of the run.
                     (above (and (> line 0)
                                 (gethash (car (nth (1- line) items))
                                          facing-places)))
                     (below (and (< last lines)
                                 (gethash (car (nth last items))
                                          facing-places)))
                     (at (1+ (or above 0))))
                (push (if (eq side 'left)
                          (list first count at 0)
                        (list at 0 first count))
                      sections)
                (when (and (or above (zerop line))
                           (or (null below) (= below at)))
                  (push (list facing at count first) pads))
                (setq line last)))))))
    ;; And the stretches both sides have, which differ in what they are drawn
    ;; as rather than in whether they are there: a section with lines on both
    ;; sides, which is what a changed hunk looks like.
    (let ((line 0)
          (lines (length left-items)))
      (while (< line lines)
        (let* ((item (nth line left-items))
               (facing (gethash (car item) right-places)))
          (if (or (null facing) (eq (cdr item) 'shared))
              (setq line (1+ line))
            (let ((last (1+ line)))
              (while (and (< last lines)
                          (let ((next (nth last left-items)))
                            (and (not (eq (cdr next) 'shared))
                                 (eql (gethash (car next) right-places)
                                      (+ facing (- last line))))))
                (setq last (1+ last)))
              (push (list (1+ line) (- last line) facing (- last line))
                    sections)
              (setq line last))))))
    (cons (sort sections (lambda (one other) (< (car one) (car other))))
          pads)))

(defvar align-buffer-fuzz-settings nil
  "A settings list to use for every seed, or nil to draw one per seed.
Pinning them is how a case found under one configuration is kept: the shape of
the buffers follows from the seed alone, but where the windows end up follows
from these as well, so the same walk under different ones goes elsewhere.")

(defun align-buffer-fuzz--settings ()
  "Return the scrolling settings for this seed, as a list to be destructured.
Each of them is something redisplay reads while deciding where to put a window,
and a reader's own configuration differs in all of them.

Drawn from a generator of its own, started from the state the walk has reached,
so that every seed's script, notes and walk are what they were before these
were drawn at all."
  (or align-buffer-fuzz-settings
      (let ((align-buffer-fuzz--state
             (1+ (mod (* 31 align-buffer-fuzz--state) 2147483646))))
        (list (align-buffer-fuzz--pick '(nil nil nil 2 5))
              (align-buffer-fuzz--pick '(0 1 10 101 101))
              (zerop (align-buffer-fuzz--random 2))
              (align-buffer-fuzz--pick '(0 0 0 1))
              (align-buffer-fuzz--pick '(2 2 2 0 5))
              (align-buffer-fuzz--pick '(0.25 0.25 0.25 0.4))
              (not (zerop (align-buffer-fuzz--random 4)))))))

(defun align-buffer-fuzz--session ()
  "Build a session from the generator's next draws, or nil when it has no pairs."
  (dolist (session (align-buffer-sessions)) (align-buffer-quit session))
  (pcase-let* ((`(,left-items ,right-items) (align-buffer-fuzz--script))
               (scale (align-buffer-fuzz--pick '(1.3 1.8 2.4)))
               (wrapped (zerop (align-buffer-fuzz--random 2)))
               (numbered (zerop (align-buffer-fuzz--random 2)))
               (margin (align-buffer-fuzz--pick '(0 2 5 5 8)))
               (`(,spacing ,conservatively ,preserve ,step ,context ,most ,exact)
                (align-buffer-fuzz--settings))
               (pairs (align-buffer-fuzz--pairs left-items right-items)))
    (when pairs
      ;; The settings a reader's own configuration would differ in, each one of
      ;; them something redisplay reads while deciding where to put a window.
      (setq-default scroll-conservatively conservatively)
      (setq-default scroll-preserve-screen-position preserve)
      (setq-default scroll-step step)
      (setq-default next-screen-context-lines context)
      (setq maximum-scroll-margin most)
      (setq align-buffer-exact-alignment exact)
      (let* ((left (align-buffer-fuzz--buffer "*align-fuzz left*" 'left
                                              left-items scale wrapped numbered
                                              margin spacing))
             (right (align-buffer-fuzz--buffer "*align-fuzz right*" 'right
                                               right-items scale wrapped
                                               numbered margin spacing))
             (runs (align-buffer-fuzz--runs left-items right-items))
             ;; Three seeds in four hang the rows a consumer would hang, so a
             ;; walk over a correspondence with holes in it is drawn both
             ;; ways, reading the holes as rows and reading over them, and
             ;; mostly the way a consumer that wants them whole reads it.
             (padded (and (cdr runs)
                          (not (zerop (align-buffer-fuzz--random 4))))))
        (let ((notes nil))
          (dotimes (_ (align-buffer-fuzz--between 2 6))
            (let* ((buffer (if (zerop (align-buffer-fuzz--random 2)) left right))
                   (side (if (eq buffer left) 'left 'right))
                   (line (align-buffer-fuzz--between
                          1 (align-buffer-fuzz--lines buffer)))
                   (rows (if (zerop (align-buffer-fuzz--random 4))
                             (align-buffer-fuzz--between 20 90)
                           (align-buffer-fuzz--between 1 3)))
                   (scaled (zerop (align-buffer-fuzz--random 3))))
              ;; Not where a pad of this seed's hangs: a note hangs at the
              ;; beginning of its line and a pad at the end of the line
              ;; before, which are the same boundary, and rows standing for
              ;; lines cannot be told from rows standing for a note once both
              ;; are drawn there.
              (unless (and padded
                           (member (list side line) (mapcar (lambda (pad)
                                                              (seq-take pad 2))
                                                            (cdr runs))))
                (align-buffer-fuzz--note buffer line rows scaled)
                (push (format "%s:%s %s rows%s"
                              (if (eq buffer left) "left" "right") line rows
                              (if scaled " tall" ""))
                      notes))))
          (setq align-buffer-fuzz--shape
                (format (concat "%s lines left and %s right, %s pairs, %s, %s, "
                                "tall at %s | margin %s of at most %s, spacing "
                                "%s, conservatively %s, preserve %s, step %s, "
                                "context %s, exact %s | %s section(s), %s | "
                                "notes %s")
                        (align-buffer-fuzz--lines left)
                        (align-buffer-fuzz--lines right)
                        (length pairs)
                        (if wrapped "wrapped" "truncated")
                        (if numbered "numbered" "unnumbered")
                        scale margin most spacing conservatively preserve step
                        context exact
                        (length (car runs))
                        (if padded
                            (format "%s pad(s)" (length (cdr runs)))
                          "no pads")
                        (mapconcat #'identity (nreverse notes) ", "))))
        (let ((session (align-buffer-show left right pairs)))
          (align-buffer-set-sections session (car runs))
          (when padded
            (dolist (pad (cdr runs))
              (pcase-let ((`(,side ,line ,rows ,facing-line) pad))
                (let ((buffer (if (eq side 'left) left right))
                      (facing (if (eq side 'left) right left)))
                  (align-buffer-pad-add
                   buffer (align-buffer--section-position buffer line) rows
                   'align-buffer-fuzz-pad facing facing-line))))
            (dolist (buffer (list left right))
              (with-current-buffer buffer (align-buffer-navigation-mode 1))))
          session)))))


;;; Reading the two windows back

(defun align-buffer-fuzz--drawn (window line)
  "Return where WINDOW draws LINE, in pixels below its top, or nil when above it.
Measured with the window's buffer current and through `window-text-pixel-size',
which takes the window's vscroll off and so answers where the line is drawn.
`posn-at-point' answers against the selected window and lies about the other."
  (with-current-buffer (window-buffer window)
    (let ((start (window-start window))
          (position (align-buffer-line-position (current-buffer) line)))
      (and (>= position start)
           (+ (cdr (window-text-pixel-size window start position))
              (align-buffer-fuzz--uncounted window start position))))))

(defun align-buffer-fuzz--uncounted (window start position)
  "Return the height of the pad rows WINDOW draws below START without counting.
A start can sit inside the one character a pad's rows hang on, and the whole of
that pad is then drawn below it unmeasured.  Worked out here rather than asked
of the package, so that the harness says where a line is drawn without taking
the word of what it is checking."
  (let* ((here (align-buffer-pad-at start))
         (next (and (< start (point-max)) (align-buffer-pad-at (1+ start))))
         ;; The pad hanging at START: the one in front of the line after it,
         ;; or - at the beginning of the buffer, where there is no line before
         ;; to hang after - the one in front of START's own line.
         (pad (cond ((and here (= (align-buffer-pad-hangs here) start)) here)
                    ((and next (= (align-buffer-pad-hangs next) start)) next))))
    (if (and pad (>= position (align-buffer-pad-anchor pad)))
        (align-buffer-pad-height window pad)
      0)))

(defun align-buffer-fuzz--text-drawn (window line)
  "Return where WINDOW draws the text of LINE, in pixels below its top.

The span to the line already reaches past whatever hangs in front of its text:
measured against `posn-at-point', a line with a note of three rows on it reads
as the row its own text is on and not as the note's first row.  What a row
standing for a line has to be level with is that text row, so nothing is added
here."
  (align-buffer-fuzz--drawn window line))

(defun align-buffer-fuzz--row-drawn (window pad row)
  "Return where WINDOW draws the facing row ROW of PAD stands for.

A pad has a row for every screen row the run is drawn as, so the row to be
level with is that far down the line's own display: the line's text, and then
the rows of it the reader has already gone past."
  (let* ((line (align-buffer-pad-facing-line pad row))
         (text (and line (align-buffer-pad-text-row pad line)))
         (drawn (and line (align-buffer-fuzz--text-drawn window line))))
    (when (and drawn text)
      (+ drawn (* (- row text) (align-buffer--row-height window))))))

(defun align-buffer-fuzz--pad-reading (session leader side)
  "Return what SESSION promises to keep level besides a pair, or nil.

As (WHAT LEADER-PIXELS FOLLOWER-PIXELS).  Inside a run of lines the other side
has not got, the pairs say nothing at all: what belongs beside those lines is
the rows of the pad standing for them, and the promise is that a row and its
own line are drawn on the same pixel.  Two ways round, by which pane the reader
is standing in: reading the rows, or reading the lines they stand for."
  (let* ((buffer (window-buffer leader))
         (follower (align-buffer--follower session side))
         (walked (buffer-local-value 'align-buffer--walked buffer)))
    (when (window-live-p follower)
      (if walked
          (pcase-let* ((`(,pad . ,row) walked)
                       (line (align-buffer-pad-facing-line pad row)))
            (when (and line (eq (overlay-get pad 'align-buffer-pad-facing)
                                (window-buffer follower)))
              (let ((own (+ (align-buffer--pad-top leader pad)
                            (align-buffer--pad-above leader pad row))))
                (list (format "row %s of the pad against line %s" row line)
                      own
                      (align-buffer-fuzz--row-drawn follower pad row)
                      ;; How near any start of the follower's own could draw
                      ;; that line, which is all it can be held to: a line too
                      ;; near the beginning of its buffer to be drawn that far
                      ;; down is one no view of its own reaches.
                      (align-buffer-fuzz--best-achievable follower line
                                                          own)))))
        (let* ((line (align-buffer-line-at buffer (window-point leader)))
               (facing (and line (align-buffer--facing-pad session side line
                                                           leader))))
          (when facing
            (pcase-let ((`(,pad . ,row) facing))
              ;; The reader's own screen row, which is what the row stands
              ;; against: a line of theirs that wraps is read a row at a time
              ;; and the pad has a row for each of them.
              (list (format "row %s of line %s against row %s of the pad"
                            (- row (align-buffer-pad-text-row pad line))
                            line row)
                    (align-buffer--point-row-top leader)
                    (+ (align-buffer--pad-top follower pad)
                       (align-buffer--pad-above follower pad row))
                    0))))))))

(defun align-buffer-fuzz--pad-faults (reading)
  "Return the fault READING shows, as a list, or nil where it keeps its promise.
Nothing is promised where the leader does not draw its own side of it: a row
or a line above the top of its window is one no follower can be level with."
  (when reading
    (pcase-let ((`(,what ,own ,other ,best) reading))
      (when (and own other (>= own 0)
                 ;; Held no closer than any start of the follower's own could
                 ;; draw it, which is the rule a pair is held to as well.
                 (> (abs (- other own)) (or best 0)))
        (list (cons 'pad-level
                    (format (concat "%s: the leader draws its own at %s px, "
                                    "the follower the other at %s px, %s px "
                                    "apart, where a start of its own comes "
                                    "within %s px")
                            what own other (- other own) (or best 0))))))))

(defun align-buffer-fuzz--above (window line)
  "Return the height of WINDOW's text above LINE, in pixels."
  (with-current-buffer (window-buffer window)
    (align-buffer--pixels-between
     window (point-min) (align-buffer-line-position (current-buffer) line))))

(defun align-buffer-fuzz--best-achievable (window line pixels)
  "Return how near PIXELS any start of WINDOW can draw LINE, in pixels.

Zero where some start draws it exactly there, and the shortfall where none
does.  A window start is a buffer position, so the places a line can be drawn
at are as far apart as the rows above it, and a block of overlay rows on one
side leaves a gap no start falls inside: the start before the block draws the
line a blockful too low and the start after it a blockful too high.  A lift
closes part of such a gap from above, by as much as a windowful while
`align-buffer-exact-alignment' is on and not at all while it is off, and
nothing closes the rest.

So this is what the follower can be held to: doing as well as some start of its
own would, rather than being level come what may."
  (let ((bound (if align-buffer-exact-alignment
                   (window-body-height window t)
                 0)))
    (with-current-buffer (window-buffer window)
      (let ((position (align-buffer-line-position (current-buffer) line))
            (best nil)
            (done nil)
            (spent 0))
        (save-excursion
          (goto-char position)
          (while (not done)
            (let* ((span (align-buffer--pixels-between window (point) position))
                   ;; A start whose span is short draws the line too high and a
                   ;; lift only takes it higher, so the shortfall is the whole
                   ;; difference; a span longer than the pixel asked for plus
                   ;; the lift leaves what the lift cannot close.
                   (out (cond ((< span pixels) (- pixels span))
                              ((> (- span bound) pixels) (- span bound pixels))
                              (t 0))))
              (when (or (null best) (< out best)) (setq best out))
              ;; Further back only lengthens the span, so once it is past what
              ;; the lift can close there is nothing better to find.
              ;; Bounded at eighty lines rather than four hundred: each
              ;; candidate costs a measurement across the span, and on two
              ;; panes that are rarely level this walk is the whole cost of a
              ;; run.  A walk that stops early can only report a bound wider
              ;; than the true one, which makes the check the more forgiving
              ;; of the two.
              (setq done (or (zerop out)
                             (> (- span bound) pixels)
                             (bobp)
                             (> spent 80)))
              (unless done (forward-line -1) (setq spent (1+ spent))))))
        (or best 0)))))

(defun align-buffer-fuzz--pinned-p (window)
  "Return non-nil when WINDOW has nothing left to give at its top."
  (and (= (window-start window)
          (with-current-buffer (window-buffer window) (point-min)))
       (zerop (window-vscroll window t))))

(defun align-buffer-fuzz--position (window)
  "Return what WINDOW is showing and where its point is."
  (list (window-start window) (window-vscroll window t) (window-point window)))

(defun align-buffer-fuzz--line-height (window position)
  "Return the pixel height of the display of POSITION's line in WINDOW.
Measured here rather than asked of align-buffer, so that one walk can be run
against two builds of it and the two counts compared."
  (with-current-buffer (window-buffer window)
    (save-excursion
      (goto-char position)
      (let* ((start (line-beginning-position))
             (next (progn (forward-line 1) (point))))
        ;; Less what hangs at the next line, which is counted at either edge
        ;; of a span and belongs to that line rather than to this one.
        (max 1 (- (cdr (window-text-pixel-size window start next))
                  (cdr (window-text-pixel-size window next next))))))))

(defun align-buffer-fuzz--busy-p ()
  "Return non-nil while align-buffer still has something of its own queued."
  (or align-buffer--pending-timer
      align-buffer--pending-place
      (bound-and-true-p align-buffer--resize-timer)
      (bound-and-true-p align-buffer--wait-timer)
      (bound-and-true-p align-buffer--pending-paint)))

(defun align-buffer-fuzz--where ()
  "Return where every window of the frame sits."
  (mapcar (lambda (window)
            (list window (window-start window) (window-vscroll window t)
                  (window-point window)))
          (window-list)))

(defun align-buffer-fuzz--settle (&optional follower)
  "Let the timers and the redisplay they ask for run, until nothing is left.
Return how many different positions FOLLOWER held while they did, which is how
jumpy the step was on the way to where it came to rest.

Waiting for a round that moves nothing is not enough on its own: a placement
queued on a timer has moved nothing yet, and a round shorter than the timer
reads that as rest.  So we wait for the queue to empty as well, and sleep for
longer than the delay a lift waits.  A check made before then is a check of a
pane still on its way."
  (let ((seen nil)
        (before nil)
        (quiet 0)
        (rounds 0))
    ;; Sampled before the first redisplay as well, since the flicker a reader
    ;; sees is between where the command left the follower and where the
    ;; placement the command asked for puts it.
    (when (window-live-p follower)
      (push (align-buffer-fuzz--position follower) seen))
    ;; Two quiet rounds, not one: redisplay can hold a correction of its own
    ;; back for a further pass - a window showing the end of its buffer with
    ;; room to spare is filled a pass after it is asked to - and a round that
    ;; moved nothing is not yet proof that nothing is going to.
    (while (and (< rounds 20)
                (or (align-buffer-fuzz--busy-p) (< quiet 2)))
      (setq before (align-buffer-fuzz--where))
      (redisplay t)
      (sit-for (+ align-buffer--lift-delay 0.01))
      (setq quiet (if (equal before (align-buffer-fuzz--where))
                      (1+ quiet)
                    0))
      (when (window-live-p follower)
        (let ((position (align-buffer-fuzz--position follower)))
          ;; A position coming round again is the follower moving away and
          ;; back, which is what a reader sees as a jump rather than as the
          ;; two panes settling.  Counted apart from how many positions it
          ;; held, since one more position on the way is not a jump and a
          ;; return always is.
          (when (and (not (equal position (car seen)))
                     (member position seen))
            (setq align-buffer-fuzz--reversals
                  (1+ align-buffer-fuzz--reversals)))
          (unless (member position seen) (push position seen))))
      (setq rounds (1+ rounds)))
    (length seen)))


;;; What a move to a section promises, checked against what is drawn

(defun align-buffer-fuzz--section-ahead (session forward)
  "Return the section a move FORWARD or back would go to in SESSION.
Found the way the command finds it, and found before the move: a session keeps
no note of which section it last used, and two sections of a random diff can
begin at the same line, one of them with no lines on this side at all."
  (let ((side (align-buffer-side))
        (sections (align-buffer-session-sections session)))
    (and side sections
         (align-buffer--section-toward
          (align-buffer--reader-place sections side) sections side forward))))

(defun align-buffer-fuzz--centring-faults (section)
  "Return the fault where SECTION, just moved to, is not drawn centred.
What is centred is the section as the leading pane shows it: its own lines
where it has them, and the rows of the pad standing for them where it has
none.  Two things are promised short of the centre: a section too near the
beginning of the buffer to be brought that far down, and one whose first line
has more hanging in front of it than the centre is deep, which leaves the
window held against what hangs."
  (let* ((window (selected-window))
         (buffer (window-buffer window))
         (side (buffer-local-value 'align-buffer--side buffer)))
    (when (and section side)
      (let* ((walked (buffer-local-value 'align-buffer--walked buffer))
             (rows (if walked (align-buffer-pad-rows (car walked))
                     (align-buffer-section-count section side)))
             (centre (if walked
                         (align-buffer--centre-pixels-of
                          window (align-buffer-pad-height window
                                                          (car walked)))
                       (align-buffer--centre-pixels window rows)))
             (hanging (if walked 0
                        (align-buffer--hanging-height
                         window (align-buffer--section-position
                                 buffer (align-buffer-section-line section
                                                                   side)))))
             (wanted (max centre hanging))
             (drawn (if walked (align-buffer--pad-top window (car walked))
                      (align-buffer-fuzz--text-drawn
                       window (align-buffer-section-line section side))))
             (at-top (with-current-buffer buffer
                       (= (window-start window) (point-min)))))
        (when (and drawn (/= drawn wanted) (not (and at-top (< drawn wanted))))
          (list (cons 'off-centre
                      (format (concat "the move left a section of %s row(s) "
                                      "drawn at %s, centred being %s")
                              rows drawn wanted))))))))


;;; What the sync promises, checked against what is drawn

(defun align-buffer-fuzz--check (session &optional swapped)
  "Return the promises SESSION's two windows are not keeping, as a list of
NAME and sentence pairs.

SWAPPED says the step just selected the other pane, which leaves the pane now
following where the reader had it: selecting a window asks for no placement, so
its point is promised nothing until the next move, and the level it already had
is all that is checked.  Where the pane the reader left was on a line with no
counterpart, the point it kept can be a line off the pair, and the next move
brings it back."
  (let* ((leader (selected-window))
         (side (buffer-local-value 'align-buffer--side (window-buffer leader)))
         (follower (and side (align-buffer--follower session side)))
         (faults nil))
    (when (and side (window-live-p follower))
      (let* ((reading (align-buffer-fuzz--pad-reading session leader side))
             (line (align-buffer-line-at (window-buffer leader)
                                         (window-point leader)))
             ;; Inside a run of lines the other side has not got, the rows
             ;; standing for them are what is level and the pairs say nothing,
             ;; so that promise is checked in place of this one - and where
             ;; the rows cannot be level with those lines at all, neither
             ;; promise holds and the reading says so.
             (anchor (and line (null reading)
                          (align-buffer-anchor session side line))))
        (when reading (cl-incf align-buffer-fuzz--padded))
        (setq faults (append faults (align-buffer-fuzz--pad-faults reading)))
        (when anchor
          (let* ((own (align-buffer-fuzz--drawn leader (car anchor)))
                 (other (align-buffer-fuzz--drawn follower (cdr anchor)))
                 (height (align-buffer--row-height follower))
                 (rows (max 1 (window-body-height follower)))
                 (margin (align-buffer-effective-margin follower)))
            ;; Where the leader does not draw the pair itself - its point is in
            ;; a run of lines with no counterpart, far enough below the pair
            ;; governing them that the pair is off the top - there is no row to
            ;; hold the follower to, and nothing is promised.
            ;;
            ;; Off the bottom says the same thing: a pair the leader draws
            ;; below its own last row is one no follower can be level with,
            ;; the follower being held to the window it has.
            ;;
            ;; And off the top in pixels, which a lift does: the line a lifted
            ;; window starts on is drawn partly or wholly above the top, so a
            ;; position at or after the start can still be somewhere no
            ;; follower could put anything.
            (when (and own (or (< own 0)
                               (>= own (window-body-height leader t))))
              (setq own nil))
            (unless own (cl-incf align-buffer-fuzz--unanchored))
            ;; Level, or as near as any start of the follower's could be.  The
            ;; walk over its starts costs a measurement each, so it is asked for
            ;; only where the two are not already level, which nearly every
            ;; step is.
            (when own
              (unless (and other (= own other))
                (let ((best (align-buffer-fuzz--best-achievable
                             follower (cdr anchor) own)))
                  (when (> best 0) (cl-incf align-buffer-fuzz--unreachable))
                  (cond
                   ((null other)
                    (push (cons 'level
                                (format (concat "the leader draws line %s at %s "
                                                "px but the follower keeps line "
                                                "%s above its own top, where a "
                                                "start of its own comes within "
                                                "%s px")
                                        (car anchor) own (cdr anchor) best))
                          faults))
                   ((> (abs (- other own)) best)
                    (push (cons 'level
                                (format (concat "not level: the leader draws "
                                                "line %s at %s px, the follower "
                                                "draws line %s at %s px, %s px "
                                                "apart, where a start of its own "
                                                "comes within %s px")
                                        (car anchor) own (cdr anchor) other
                                        (- other own) best))
                          faults))))))
            ;; Point on the pair, or on the row nearest it the band allows.
            (when (and own other (not swapped))
              (let* ((row (round other height))
                     ;; No floor when the window shows its first line, which is
                     ;; the rule the placement itself goes by: the margin is
                     ;; there to stop redisplay scrolling back, and there is
                     ;; nothing above the first line to scroll into.
                     (lowest (if (= (window-start follower)
                                    (with-current-buffer (window-buffer follower)
                                      (point-min)))
                                 0 margin))
                     (highest (max 0 (- rows margin 1)))
                     (wanted (max 0 (min (1- rows)
                                         (max lowest (min highest row)))))
                     (at (align-buffer-line-at (window-buffer follower)
                                               (window-point follower)))
                     (drawn (and at (align-buffer-fuzz--drawn follower at)))
                     (point-row (and drawn (round drawn height)))
                     ;; How many rows point's own line is drawn as: point can
                     ;; only sit on a line, and a line taller than a row
                     ;; covers the row the band asks for without beginning on
                     ;; it, which is as near as the band can be met.
                     (point-rows
                      (and drawn
                           (max 1 (round (align-buffer-fuzz--line-height
                                          follower (window-point follower))
                                         height)))))
                (cond
                 ;; Nothing is promised while a pad's rows are being read on
                 ;; either side.  The pairs say nothing there: the pane whose
                 ;; rows they are holds point on the line they hang on, and
                 ;; the pane facing it holds point on the line a row stands
                 ;; for, which is a line off the pair by as many rows as the
                 ;; reader has walked.
                 ((align-buffer-fuzz--reading-rows-p leader follower) nil)
                 ;; Nothing is promised where the band asks for a row the
                 ;; buffer runs out before: point can be put on a line and
                 ;; the follower has none below its last, so the nearest it
                 ;; can come is the end of its own text.
                 ((let ((reach (cdr (align-buffer--position-rows-below
                                     follower
                                     (align-buffer-line-position
                                      (window-buffer follower) (cdr anchor))
                                     (- wanted row)))))
                    (/= reach (- wanted row)))
                  nil)
                 ((null point-row)
                  (push (cons 'point
                              (format (concat "the follower's point is on line "
                                              "%s, which it keeps above its own "
                                              "top")
                                      at))
                        faults))
                 ((and (<= point-row wanted)
                       (< wanted (+ point-row point-rows)))
                  nil)
                 ((= wanted row)
                  (push (cons 'point
                              (format (concat "the pair is drawn on row %s, "
                                              "which the band of %s..%s allows, "
                                              "yet point is on row %s (line %s "
                                              "for line %s)")
                                      row lowest highest point-row at
                                      (cdr anchor)))
                        faults))
                 ;; Point on the first line the band allows, which is not
                 ;; always the line the row arithmetic names: the rows are
                 ;; divided by one height while the lines are drawn at the
                 ;; heights they have, so the row asked for can fall between
                 ;; two of them and the one below it is as near the pair as
                 ;; point can come.
                 ((let ((line (align-buffer--line-below follower
                                                        (* wanted height))))
                    (and line (= line (window-point follower))))
                  nil)
                 (t
                  (push (cons 'clamp
                              (format (concat "the pair is drawn on row %s, "
                                              "outside the band of %s..%s, so "
                                              "point belongs on row %s, and it "
                                              "is on row %s")
                                      row lowest highest wanted point-row))
                        faults)))))))))
    faults))

(defun align-buffer-fuzz--reading-rows-p (leader follower)
  "Return non-nil while either pane is reading a row of a pad.
The reader's own pane reads the rows standing for lines it has not got, and
the pane facing them is held on the line each row stands for, so neither pane
has its point where the pairs would put it."
  (or (buffer-local-value 'align-buffer--walked (window-buffer leader))
      (and (window-live-p follower)
           (buffer-local-value 'align-buffer--walked
                               (window-buffer follower)))))

(defun align-buffer-fuzz--settled-check (session)
  "Return a fault when placing SESSION's follower again moves it.
The placement is a real one, so where it moves the follower the walk goes on
from there.  That is the state the sync itself produced, and a second placement
from a leader that has not moved is asked for by anything that syncs without a
command behind it: a notch over the follower, a redisplay that scrolls either
window, a command that leaves the leader's line alone."
  (let* ((leader (selected-window))
         (side (buffer-local-value 'align-buffer--side (window-buffer leader)))
         (follower (and side (align-buffer--follower session side))))
    (when (window-live-p follower)
      (let ((before (align-buffer-fuzz--position follower))
            ;; A placement asked for while the rows are being read would take
            ;; point off the row the reader is on, which is the walk's to
            ;; hold: nothing is promised about placing again there.
            (reading (align-buffer-fuzz--reading-rows-p leader follower)))
        (align-buffer-sync leader)
        (align-buffer-fuzz--settle)
        (let ((after (align-buffer-fuzz--position follower)))
          (unless (or reading (equal before after))
            (list (cons 'settled
                        (format (concat "placing the follower again moved it, "
                                        "from start %s vscroll %s point %s to "
                                        "start %s vscroll %s point %s")
                                (nth 0 before) (nth 1 before) (nth 2 before)
                                (nth 0 after) (nth 1 after) (nth 2 after))))))))))


;;; The walk

(defconst align-buffer-fuzz--moves
  '(down down down down up up up next prior wheel-down wheel-down wheel-up
         scroll-row-down scroll-row-down scroll-row-up top bottom recenter
         jump jump lead forward back section-down section-down section-up
         wider narrower wider-held narrower-held)
  "The steps a walk is drawn from, named repeatedly to weight the draw.")

(defvar align-buffer-fuzz-line 1
  "The line the next jump of the walk goes to.")

(defvar align-buffer-fuzz-row 0
  "The row the next recentre of the walk puts the line point is on.")

;; A jump and a recentre as COMMANDS, bound to keys the walk presses, so that
;; the command loop and every hook on it run the way they do for a reader.
;; Called out of Lisp instead, they leave out what makes a pad behave: the
;; check after a command is what gives the cursor to a row and holds the panes
;; while a run is read.

(defun align-buffer-fuzz-goto-line ()
  "Go to the line the walk has drawn."
  (interactive)
  (goto-char (align-buffer-line-position (current-buffer)
                                         align-buffer-fuzz-line)))

(defun align-buffer-fuzz-recenter ()
  "Put the line point is on the row the walk has drawn."
  (interactive)
  (recenter align-buffer-fuzz-row))

(defun align-buffer-fuzz--wheel (window direction)
  "Send one wheel notch of DIRECTION over WINDOW."
  (let ((position (list window (window-start window) '(0 . 0) 0)))
    (execute-kbd-macro (vector (list direction position 1)))))

(defvar align-buffer-fuzz-columns 4
  "How many columns the next resize of the walk moves the divider by.")

(defconst align-buffer-fuzz--least-columns 24
  "A pane is left this many columns wide, and is not narrowed below it.")

(defun align-buffer-fuzz-widen-pane ()
  "Make the window point is in wider."
  (interactive)
  (enlarge-window-horizontally align-buffer-fuzz-columns))

(defun align-buffer-fuzz-narrow-pane ()
  "Make the window point is in narrower."
  (interactive)
  (shrink-window-horizontally align-buffer-fuzz-columns))

(defun align-buffer-fuzz--beside (window)
  "Return the pane facing WINDOW, which is the other window of its frame."
  (car (delq window (window-list (window-frame window) 'no-minibuffer))))

(defun align-buffer-fuzz--columns (window grow)
  "Return how many columns to resize WINDOW by, widening it when GROW.
Nil where the pane that would give up the columns has none to spare.

The draw is made whether or not the resize happens, so that the walk follows
from the seed and not from how wide the panes happen to stand."
  (let* ((columns (align-buffer-fuzz--between 1 8))
         (beside (align-buffer-fuzz--beside window))
         (giving (if grow beside window))
         (room (and (window-live-p beside)
                    (- (window-body-width giving)
                       align-buffer-fuzz--least-columns))))
    (when (and room (> room 0))
      (min room columns))))

(defun align-buffer-fuzz--resize (grow presses)
  "Press the key resizing the pane the reader is in PRESSES times.
GROW says it widens the pane rather than narrows it.  Return what to call the
step, and refuse the step where neither pane has a column to spare.

What a repeating key gives: each press is a size change of its own, so the
measure a size change asks for falls between two presses while more resizing
is still on its way."
  (let ((taken 0))
    (dotimes (_ presses)
      (when-let ((columns (align-buffer-fuzz--columns (selected-window) grow)))
        (setq align-buffer-fuzz-columns columns)
        (execute-kbd-macro (kbd (if grow "C-c w" "C-c s")))
        (setq taken (+ taken columns))
        (when (> presses 1)
          ;; Between the presses, so the hook sees each size change and what
          ;; it queues runs while the next press is still to come.
          (redisplay t)
          (sit-for 0.01))))
    (when (zerop taken)
      (user-error "No pane with a column to spare"))
    (format "%s the pane by %s column(s), %s press(es)"
            (if grow "widen" "narrow") taken presses)))

(defun align-buffer-fuzz--move (move)
  "Take one MOVE in SESSION and return what to call it.
The steps that stand for a key go in as real input, so the command runs and
redisplay answers it the way it answers a reader.  The two that no key gives -
a jump to a line and a recenter - ask for the placement themselves."
  (let ((window (selected-window)))
    (pcase move
      ('down (execute-kbd-macro (kbd "<down>")) "down a line")
      ('up (execute-kbd-macro (kbd "<up>")) "up a line")
      ('next (execute-kbd-macro (kbd "<next>")) "down a page")
      ('prior (execute-kbd-macro (kbd "<prior>")) "up a page")
      ('scroll-row-down (execute-kbd-macro (kbd "M-<down>")) "scroll down a row")
      ('scroll-row-up (execute-kbd-macro (kbd "M-<up>")) "scroll up a row")
      ('wheel-down (align-buffer-fuzz--wheel window 'wheel-down) "a wheel notch down")
      ('wheel-up (align-buffer-fuzz--wheel window 'wheel-up) "a wheel notch up")
      ('top (execute-kbd-macro (kbd "M-<")) "to the first line")
      ('bottom (execute-kbd-macro (kbd "M->")) "to the last line")
      ('forward (execute-kbd-macro (kbd "<right>")) "forward a character")
      ('back (execute-kbd-macro (kbd "<left>")) "back a character")
      ;; Through the keys they are bound to in these buffers, so the command
      ;; loop and its hooks run as they do for every other step.
      ('section-down (execute-kbd-macro (kbd "C-c n")) "to the next section")
      ('section-up (execute-kbd-macro (kbd "C-c p")) "to the previous section")
      ('recenter
       (setq align-buffer-fuzz-row
             (align-buffer-fuzz--random (window-body-height window)))
       (execute-kbd-macro (kbd "C-c l"))
       (format "recenter on row %s" align-buffer-fuzz-row))
      ('jump
       (setq align-buffer-fuzz-line
             (align-buffer-fuzz--between
              1 (align-buffer-fuzz--lines (window-buffer window))))
       (execute-kbd-macro (kbd "C-c g"))
       (format "jump to line %s" align-buffer-fuzz-line))
      ('wider (align-buffer-fuzz--resize t 1))
      ('narrower (align-buffer-fuzz--resize nil 1))
      ('wider-held (align-buffer-fuzz--resize
                    t (align-buffer-fuzz--between 2 5)))
      ('narrower-held (align-buffer-fuzz--resize
                       nil (align-buffer-fuzz--between 2 5)))
      ('lead
       ;; Through the command, since selecting a window from Lisp asks for no
       ;; placement and would leave the new follower wherever it last stood.
       (execute-kbd-macro (kbd "C-x o"))
       "lead from the other side"))))

(defun align-buffer-fuzz--step (session seed number)
  "Take step NUMBER of SEED's walk over SESSION and return what it found."
  (let* ((move (align-buffer-fuzz--pick align-buffer-fuzz--moves))
         (was (selected-window))
         (label nil)
         (refused nil)
         (faults nil)
         ;; Before the move, which is the only time the section it will use
         ;; can be told apart from another one beginning at the same line.
         (going (and (memq move '(section-down section-up))
                     (align-buffer-fuzz--section-ahead
                      session (eq move 'section-down)))))
    (condition-case error
        (setq label (align-buffer-fuzz--move move))
      ;; An arrow at the last line signals, which is Emacs and not the sync.
      ((beginning-of-buffer end-of-buffer)
       (setq label (format "%s, at the edge" move)))
      ;; And a move with nowhere to go says so, which is the command keeping
      ;; its own promise rather than a fault of the sync's.
      (user-error
       (setq refused t)
       (setq label (format "%s, refused: %s" move
                           (error-message-string error))))
      (error
       (setq label (format "%s" move))
       (push (cons 'signal (format "signalled %s" (error-message-string error)))
             faults)))
    (push (align-buffer-fuzz--settle
           (align-buffer--follower
            session (buffer-local-value 'align-buffer--side
                                        (window-buffer (selected-window)))))
          align-buffer-fuzz--jumpy)
    ;; A move to a section promises where the section is drawn as well as what
    ;; is level with it, and that promise is this move's alone: the step after
    ;; it is free to leave the section anywhere.
    (when (and going (not refused))
      (setq faults (append faults
                           (align-buffer-fuzz--centring-faults going))))
    (let ((swapped (not (eq was (selected-window)))))
      (setq faults (append faults (align-buffer-fuzz--check session swapped)))
      (when align-buffer-fuzz-ask-again
        (setq faults
              (append faults
                      (if swapped
                          ;; Selecting a pane asks for no placement, so one is
                          ;; asked for here, which is what the reader's next key
                          ;; does, and the promises are held against what it
                          ;; produced rather than against its having moved.
                          (progn (align-buffer-sync (selected-window))
                                 (align-buffer-fuzz--settle)
                                 (align-buffer-fuzz--check session))
                        (align-buffer-fuzz--settled-check session))))))
    ;; And once more after letting everything come to rest again.  A
    ;; placement made while the leader was still settling - a point taken to
    ;; the end of the buffer settles over more than one pass - is corrected by
    ;; the hook that the leader's own move runs, so a fault seen at the first
    ;; look can be gone a moment later.  What is still wrong here is wrong for
    ;; good, which is what a reader is left looking at.
    ;;
    ;; ALIGN_BUFFER_FUZZ_AT_REST=no leaves it out, which is how a walk over
    ;; code that fails often enough to make the second look cost more than
    ;; the walk itself still finishes.
    (unless (equal (getenv "ALIGN_BUFFER_FUZZ_AT_REST") "no")
      (align-buffer-fuzz--settle)
      (dolist (fault (align-buffer-fuzz--check session))
        (push (format "seed %s step %s (%s) [%s] %s"
                      seed number label (car fault) (cdr fault))
              align-buffer-fuzz--at-rest)))
    (dolist (fault faults)
      (push (format "seed %s step %s (%s) [%s] %s"
                    seed number label (car fault) (cdr fault))
            align-buffer-fuzz--faults))
    faults))

(defun align-buffer-fuzz-walk (seed &optional steps)
  "Build the buffers SEED describes, walk STEPS over them, and return the faults.
Everything the walk does follows from SEED, so a fault is replayed by its seed
and step number alone."
  (align-buffer-fuzz--seed seed)
  (setq align-buffer-fuzz--faults nil)
  (setq align-buffer-fuzz--at-rest nil)
  (setq align-buffer-fuzz--reversals 0)
  (setq align-buffer-fuzz--jumpy nil)
  (setq align-buffer-fuzz--unanchored 0)
  (setq align-buffer-fuzz--padded 0)
  (setq align-buffer-fuzz--unreachable 0)
  (let ((session (align-buffer-fuzz--session)))
    (when session
      (align-buffer-fuzz--settle)
      (dotimes (step (or steps align-buffer-fuzz-steps))
        (align-buffer-fuzz--step session seed (1+ step)))
      (align-buffer-quit session)))
  (nreverse align-buffer-fuzz--faults))

(defun align-buffer-fuzz--report-file ()
  "Return where to write the report."
  (or (getenv "ALIGN_BUFFER_FUZZ_OUT")
      (expand-file-name "align-buffer-fuzz.txt" temporary-file-directory)))

(defun align-buffer-fuzz--state-of (session)
  "Return a line saying how SESSION's two windows stand."
  (let* ((leader (selected-window))
         (side (buffer-local-value 'align-buffer--side (window-buffer leader)))
         (follower (align-buffer--follower session side))
         (line (align-buffer-line-at (window-buffer leader)
                                     (window-point leader)))
         (anchor (align-buffer-anchor session side line)))
    (format (concat "lead %s line %s start %s/%s vscroll %s | follow line %s "
                    "point %s start %s/%s first %s vscroll %s | pair %s | "
                    "drawn %s and %s | above the pair %s px | band %s rows "
                    "margin %s row height %s")
            side line
            (align-buffer-line-at (window-buffer leader) (window-start leader))
            (window-start leader) (window-vscroll leader t)
            (align-buffer-line-at (window-buffer follower)
                                  (window-point follower))
            (window-point follower)
            (align-buffer-line-at (window-buffer follower)
                                  (window-start follower))
            (window-start follower)
            (with-current-buffer (window-buffer follower) (point-min))
            (window-vscroll follower t)
            anchor
            (align-buffer-fuzz--drawn leader (car anchor))
            (align-buffer-fuzz--drawn follower (cdr anchor))
            (align-buffer-fuzz--above follower (cdr anchor))
            (window-body-height follower)
            (align-buffer-effective-margin follower)
            (align-buffer--row-height follower))))

(defun align-buffer-fuzz-replay (seed step)
  "Walk SEED as far as STEP and write everything about where it got to.
Written to align-buffer-fuzz-replay.txt beside the report, so a fault the walk
reported can be read in full without guessing at the state behind it."
  (align-buffer-fuzz--seed seed)
  (setq align-buffer-fuzz--faults nil)
  (setq align-buffer-fuzz--jumpy nil)
  (setq align-buffer-fuzz--unanchored 0)
  (setq align-buffer-fuzz--padded 0)
  (setq align-buffer-fuzz--unreachable 0)
  (let ((session (align-buffer-fuzz--session))
        (lines nil))
    (when session
      (align-buffer-fuzz--settle)
      (push (format "seed %s: %s" seed align-buffer-fuzz--shape) lines)
      (push (format "  start    %s" (align-buffer-fuzz--state-of session)) lines)
      (dotimes (one step)
        (let* ((number (1+ one))
               (before (length align-buffer-fuzz--faults))
               (move (align-buffer-fuzz--step session seed number)))
          (push (format "  step %-3s %s" number
                        (align-buffer-fuzz--state-of session))
                lines)
          (when (> (length align-buffer-fuzz--faults) before)
            (dolist (fault (reverse move))
              (push (format "           [%s] %s" (car fault) (cdr fault))
                    lines)))))
      (align-buffer-quit session))
    (with-temp-file (expand-file-name
                     "align-buffer-fuzz-replay.txt"
                     (file-name-directory (align-buffer-fuzz--report-file)))
      (dolist (line (nreverse lines)) (insert line "\n")))))

;;;###autoload
(defun align-buffer-fuzz (seed)
  "Walk SEED's buffers and say in the echo area what it found."
  (interactive "nSeed: ")
  (let ((faults (align-buffer-fuzz-walk seed)))
    (message "seed %s: %s" seed
             (if faults (format "%d fault(s), the first being %s"
                                (length faults) (car faults))
               "nothing"))))

(defun align-buffer-fuzz-configure ()
  "Lay out the frame and turn the wheel on.
The scrolling settings are a seed's own, so they are set as each session is
built rather than here.

A window of middling height unless ALIGN_BUFFER_FUZZ_MAXIMIZED asks for the
screen: what a placement costs grows with the rows it may have to measure, and
a seed that walks in half a minute here takes ten in the tallest window there
is."
  (if (getenv "ALIGN_BUFFER_FUZZ_MAXIMIZED")
      (align-buffer-background-frame)
    (align-buffer-background-frame 100 40))
  (mouse-wheel-mode 1)
  (when (getenv "ALIGN_BUFFER_FUZZ_READER_ONLY")
    (setq align-buffer-fuzz-ask-again nil))
  (setq mouse-wheel-scroll-amount '(3 ((shift) . hscroll)))
  (setq mouse-wheel-progressive-speed nil))

(defun align-buffer-fuzz-batch ()
  "Walk a range of seeds, write a report, and exit with a status.
ALIGN_BUFFER_FUZZ_SEEDS gives the range as FIRST-LAST, ALIGN_BUFFER_FUZZ_STEPS
the steps per seed, ALIGN_BUFFER_FUZZ_OUT where to write, and
ALIGN_BUFFER_FUZZ_MAXIMIZED a frame as big as the screen."
  (let* ((range (or (getenv "ALIGN_BUFFER_FUZZ_SEEDS") "1-12"))
         (parts (split-string range "-" t))
         (first (string-to-number (car parts)))
         (last (string-to-number (or (cadr parts) (car parts))))
         (steps (string-to-number
                 (or (getenv "ALIGN_BUFFER_FUZZ_STEPS")
                     (number-to-string align-buffer-fuzz-steps))))
         (file (align-buffer-fuzz--report-file)))
    (run-with-idle-timer
     0.5 nil
     (lambda ()
       (align-buffer-fuzz-configure)
       (let ((lines nil)
             (counts nil)
             (jumpiest 0)
             (resting 0)
             (reversals 0)
             (total 0))
         (cl-loop for seed from first to last do
                  (let ((faults (align-buffer-fuzz-walk seed steps)))
                    (setq total (+ total (length faults)))
                    (setq resting (+ resting
                                     (length align-buffer-fuzz--at-rest)))
                    (setq reversals (+ reversals align-buffer-fuzz--reversals))
                    (setq lines (append lines align-buffer-fuzz--at-rest))
                    (setq jumpiest (max jumpiest (apply #'max 1 align-buffer-fuzz--jumpy)))
                    (dolist (one align-buffer-fuzz--jumpy)
                      (setf (alist-get one counts 0) (1+ (alist-get one counts 0))))
                    (setq lines
                          (append lines
                                  (list (format (concat "seed %-4s %s: %s "
                                                        "fault(s), %s step(s) "
                                                        "with the pair off the "
                                                        "leader's top, %s with "
                                                        "no start that could be "
                                                        "level, %s with a row "
                                                        "to be level with")
                                                seed align-buffer-fuzz--shape
                                                (length faults)
                                                align-buffer-fuzz--unanchored
                                                align-buffer-fuzz--unreachable
                                                align-buffer-fuzz--padded))
                                  faults))
                    (message "align-buffer fuzz: seed %s, %d fault(s)"
                             seed (length faults))))
         (with-temp-file file
           (insert (format "seeds %s-%s, %s steps each, %d fault(s)\n"
                           first last steps total))
           ;; Of those, the ones still there once the panes have come to rest,
           ;; which is what a reader is left looking at: a placement made
           ;; while the leader was still settling is corrected by the hook
           ;; that its own move runs, and the rest of the count is that
           ;; window of time.
           (insert (format "%d of them still there at rest\n" resting))
           (insert (format "%d step(s) where a pane moved away and back\n"
                           reversals))
           ;; The geometry, since where a walk ends up depends on it and a
           ;; maximized frame is a different size on a different screen.
           (insert (format "a frame of %s by %s, so a window of %s rows\n"
                           (frame-width) (frame-height)
                           (window-body-height (selected-window))))
           (insert (format "positions held per step: %s, the most being %s\n\n"
                           (mapconcat (lambda (one)
                                        (format "%s held by %s" (car one) (cdr one)))
                                      (sort counts #'car-less-than-car) ", ")
                           jumpiest))
           (dolist (line lines) (insert line "\n")))
         (kill-emacs (if (> total 0) 1 0)))))))

(provide 'align-buffer-fuzz)

;;; align-buffer-fuzz.el ends here
