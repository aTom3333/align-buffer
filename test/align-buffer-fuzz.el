;;; align-buffer-fuzz.el --- Random correspondences walked at random -*- lexical-binding: t; -*-

;;; Commentary:

;; A seed builds two buffers and a correspondence between them, and then a walk
;; over them: arrows, pages, wheel notches, single-row scrolls, jumps to a line,
;; and swaps of which window leads.  After every step the two windows are read
;; back and checked against what the sync promises, so a seed either passes or
;; names the step it broke on and the numbers it broke with.
;;
;; A seed decides everything: the shape of the script, the lengths, the notes,
;; the margin, whether the buffers wrap, and the walk itself.  So a failure is
;; replayed by its seed and step with nothing else recorded.
;;
;; Three promises are checked, each of which holds no matter what the buffers
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
;; How jumpy a step was is counted and reported but not judged: it is how many
;; different positions the follower held between the command and the screen
;; coming to rest.
;;
;; `M-x align-buffer-fuzz' walks one seed and reports in the echo area.  In
;; batch, `align-buffer-fuzz-batch' walks a range of them and writes a report.

;;; Code:

(require 'cl-lib)
(require 'align-buffer)

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
      ('added (format "\tadded(%d);" id))
      ('removed (format "\tremoved(%d);" id))
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
      (let ((left (align-buffer-fuzz--buffer "*align-fuzz left*" 'left
                                             left-items scale wrapped numbered
                                             margin spacing))
            (right (align-buffer-fuzz--buffer "*align-fuzz right*" 'right
                                              right-items scale wrapped numbered
                                              margin spacing)))
        (let ((notes nil))
          (dotimes (_ (align-buffer-fuzz--between 2 6))
            (let* ((buffer (if (zerop (align-buffer-fuzz--random 2)) left right))
                   (line (align-buffer-fuzz--between
                          1 (align-buffer-fuzz--lines buffer)))
                   (rows (if (zerop (align-buffer-fuzz--random 4))
                             (align-buffer-fuzz--between 20 90)
                           (align-buffer-fuzz--between 1 3)))
                   (scaled (zerop (align-buffer-fuzz--random 3))))
              (align-buffer-fuzz--note buffer line rows scaled)
              (push (format "%s:%s %s rows%s"
                            (if (eq buffer left) "left" "right") line rows
                            (if scaled " tall" ""))
                    notes)))
          (setq align-buffer-fuzz--shape
                (format (concat "%s lines left and %s right, %s pairs, %s, %s, "
                                "tall at %s | margin %s of at most %s, spacing "
                                "%s, conservatively %s, preserve %s, step %s, "
                                "context %s, exact %s | notes %s")
                        (align-buffer-fuzz--lines left)
                        (align-buffer-fuzz--lines right)
                        (length pairs)
                        (if wrapped "wrapped" "truncated")
                        (if numbered "numbered" "unnumbered")
                        scale margin most spacing conservatively preserve step
                        context exact
                        (mapconcat #'identity (nreverse notes) ", "))))
        (align-buffer-show left right pairs)))))


;;; Reading the two windows back

(defun align-buffer-fuzz--drawn (window line)
  "Return where WINDOW draws LINE, in pixels below its top, or nil when above it.
Measured with the window's buffer current and through `window-text-pixel-size',
which takes the window's vscroll off and so answers where the line is drawn.
`posn-at-point' answers against the selected window and lies about the other."
  (with-current-buffer (window-buffer window)
    (let ((position (align-buffer-line-position (current-buffer) line)))
      (and (>= position (window-start window))
           (cdr (window-text-pixel-size window (window-start window)
                                        position))))))

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
      (let* ((line (align-buffer-line-at (window-buffer leader)
                                         (window-point leader)))
             (anchor (and line (align-buffer-anchor session side line))))
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
            (when (and own (>= own (window-body-height leader t)))
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
                 (t
                  (push (cons 'clamp
                              (format (concat "the pair is drawn on row %s, "
                                              "outside the band of %s..%s, so "
                                              "point belongs on row %s, and it "
                                              "is on row %s")
                                      row lowest highest wanted point-row))
                        faults)))))))))
    faults))

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
      (let ((before (align-buffer-fuzz--position follower)))
        (align-buffer-sync leader)
        (align-buffer-fuzz--settle)
        (let ((after (align-buffer-fuzz--position follower)))
          (unless (equal before after)
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
         jump jump lead forward back)
  "The steps a walk is drawn from, named repeatedly to weight the draw.")

(defun align-buffer-fuzz--wheel (window direction)
  "Send one wheel notch of DIRECTION over WINDOW."
  (let ((position (list window (window-start window) '(0 . 0) 0)))
    (execute-kbd-macro (vector (list direction position 1)))))

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
      ('recenter
       (let ((row (align-buffer-fuzz--random (window-body-height window))))
         (recenter row)
         (align-buffer-sync window)
         (format "recenter on row %s" row)))
      ('jump
       (let* ((buffer (window-buffer window))
              (line (align-buffer-fuzz--between
                     1 (align-buffer-fuzz--lines buffer))))
         (with-current-buffer buffer
           (goto-char (align-buffer-line-position buffer line)))
         (align-buffer-sync window)
         (format "jump to line %s" line)))
      ('lead
       ;; Through the command, since selecting a window from Lisp asks for no
       ;; placement and would leave the new follower wherever it last stood.
       (execute-kbd-macro (kbd "C-x o"))
       "lead from the other side"))))

(defun align-buffer-fuzz--step (session seed number)
  "Take step NUMBER of SEED's walk over SESSION and return what it found."
  (let ((move (align-buffer-fuzz--pick align-buffer-fuzz--moves))
        (was (selected-window))
        (label nil)
        (faults nil))
    (condition-case error
        (setq label (align-buffer-fuzz--move move))
      ;; An arrow at the last line signals, which is Emacs and not the sync.
      ((beginning-of-buffer end-of-buffer)
       (setq label (format "%s, at the edge" move)))
      (error
       (setq label (format "%s" move))
       (push (cons 'signal (format "signalled %s" (error-message-string error)))
             faults)))
    (push (align-buffer-fuzz--settle
           (align-buffer--follower
            session (buffer-local-value 'align-buffer--side
                                        (window-buffer (selected-window)))))
          align-buffer-fuzz--jumpy)
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
built rather than here."
  ;; Maximized rather than given a size: a frame whose own size puts part of it
  ;; past the edge of the screen stops being redrawn, and a walk whose every
  ;; step waits on a redisplay then waits for as long as it takes someone to
  ;; drag the frame back.
  (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
  (redisplay t)
  (mouse-wheel-mode 1)
  (when (getenv "ALIGN_BUFFER_FUZZ_READER_ONLY")
    (setq align-buffer-fuzz-ask-again nil))
  (setq mouse-wheel-scroll-amount '(3 ((shift) . hscroll)))
  (setq mouse-wheel-progressive-speed nil))

(defun align-buffer-fuzz-batch ()
  "Walk a range of seeds, write a report, and exit with a status.
ALIGN_BUFFER_FUZZ_SEEDS gives the range as FIRST-LAST, ALIGN_BUFFER_FUZZ_STEPS
the steps per seed, and ALIGN_BUFFER_FUZZ_OUT where to write."
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
                                                        "level")
                                                seed align-buffer-fuzz--shape
                                                (length faults)
                                                align-buffer-fuzz--unanchored
                                                align-buffer-fuzz--unreachable))
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
