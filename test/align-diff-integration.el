;;; align-diff-integration.el --- Walking and scrolling a pad, on screen -*- lexical-binding: t; -*-

;;; Commentary:

;; What a pad does under real input, which only a real frame can answer: the
;; rows of a pad hold no position, so where the cursor goes, how far a scroll
;; takes the window, and whether the two panes still read level are all
;; questions about pixels on a screen rather than about text.
;;
;; Every scenario drives the session's own keys through `execute-kbd-macro',
;; and asks two things of the result: which row of the pad the cursor is on,
;; and whether the line that row stands for is drawn level with it in the
;; facing pane.
;;
;; Needs a graphical frame, so it is not an ert suite in batch.  Run with:
;;   emacs -Q --iconic -L . -L test -l test/align-diff-integration.el \
;;         -f align-diff-integration-batch

;;; Code:

(require 'align-diff)
(require 'align-diff-demo)
(require 'align-buffer-background)
(require 'cl-lib)

(defvar align-diff-integration--failures nil
  "Descriptions of what went wrong, newest first.")

(defvar align-diff-integration--scenarios nil
  "Scenarios to run, as (NAME . FUNCTION), in the order defined.")

(defvar align-diff-integration--scenario nil
  "The scenario running, for what it reports.")

(defmacro align-diff-integration-defscenario (name &rest body)
  "Define scenario NAME, whose BODY drives input and calls `expect'."
  (declare (indent 1))
  `(add-to-list 'align-diff-integration--scenarios
                (cons ',name (lambda () ,@body))
                t))

(defun align-diff-integration--expect (what wanted got)
  "Record that the scenario wanted WANTED for WHAT and got GOT."
  (unless (equal wanted got)
    (push (format "%s: %s: wanted %S, got %S"
                  align-diff-integration--scenario what wanted got)
          align-diff-integration--failures)))


;;; A session with one big pad in it

(defconst align-diff-integration--added 60
  "How many lines one side alone has, and so how many rows the pad is.")

(defconst align-diff-integration--at 51
  "The padded side's line the run of lines it has none of belongs before.")

(defconst align-diff-integration--below 110
  "A line standing on which leaves the whole pad above what the window shows.
Scrolling back up from there is what reaches the pad from below, the pad
being as tall as the window and so filling it as soon as point is beside
it.")

(defun align-diff-integration--text (lines)
  "Return LINES, each as its own line of text."
  (mapconcat (lambda (line) (concat line "\n")) lines ""))

(defun align-diff-integration--session (&optional padded)
  "Show two buffers differing by one run of lines, and return the session.
PADDED says which side the pad lands on: `left' by default, where the new
side alone has the run, and `right', where the old side alone still has it."
  (delete-other-windows)
  (let* ((shared (cl-loop for line from 1 to 120
                          collect (format "shared line %03d" line)))
         (run (cl-loop for line from 1 to align-diff-integration--added
                       collect (format "one side only %03d" line)))
         (at (1- align-diff-integration--at))
         (with-run (align-diff-integration--text
                    (append (seq-take shared at) run (seq-drop shared at))))
         (without-run (align-diff-integration--text shared))
         (deletion (eq padded 'right))
         (old (generate-new-buffer " *align-diff integration old*"))
         (new (generate-new-buffer " *align-diff integration new*")))
    (with-current-buffer old
      (insert (if deletion with-run without-run))
      (set-buffer-modified-p nil))
    (with-current-buffer new
      (insert (if deletion without-run with-run))
      (set-buffer-modified-p nil))
    ;; Set rather than bound: a scenario drives input after this returns, and
    ;; the session has to go on being the session these settings make.
    (setq align-diff-gap-marker 'pad)
    (setq align-diff-refine-hunks nil)
    (setq align-diff-idle-delay nil)
    (setq align-buffer-exact-alignment t)
    (let ((session (align-diff-buffers old new
                                       (list :owned-buffers (list old new)))))
      (align-diff-integration--settle)
      session)))

(defun align-diff-integration--where ()
  "Return where every window of the frame sits."
  (mapcar (lambda (window)
            (list window (window-start window) (window-vscroll window t)
                  (window-point window)))
          (window-list)))

(defun align-diff-integration--busy-p ()
  "Return non-nil while align-buffer still has something of its own queued."
  (or align-buffer--pending-timer
      align-buffer--pending-place
      align-buffer--wait-timer))

(defun align-diff-integration--settle ()
  "Let the timers and the redisplay they ask for run, until nothing is left.
Waiting for a round that moves nothing is not enough on its own: a placement
queued on a timer has moved nothing yet, and a round shorter than the timer
reads that as rest.  So we wait for the queue to empty as well, and sleep for
longer than the delay a lift waits."
  (let ((before nil)
        (rounds 0))
    (while (and (< rounds 40)
                (or (align-diff-integration--busy-p)
                    (not (equal before (align-diff-integration--where)))))
      (setq before (align-diff-integration--where))
      (redisplay t)
      (sit-for (+ align-buffer--lift-delay 0.01))
      (setq rounds (1+ rounds)))))

(defun align-diff-integration--keys (keys &optional times)
  "Run KEYS, a `kbd' string, TIMES over as real input."
  (dotimes (_ (or times 1))
    (execute-kbd-macro (kbd keys))
    (redisplay t))
  (align-diff-integration--settle))

(defun align-diff-integration--wheel (window direction &optional times)
  "Send TIMES wheel notches of DIRECTION over WINDOW."
  (dotimes (_ (or times 1))
    (let ((position (list window (window-start window) '(0 . 0) 0)))
      (execute-kbd-macro (vector (list direction position 1))))
    (redisplay t))
  (align-diff-integration--settle))

(defun align-diff-integration--click (window y)
  "Click in WINDOW at Y pixels below the top of its text."
  (let ((position (posn-at-x-y 8 (+ y (window-tab-line-height window)
                                    (window-header-line-height window))
                               window)))
    (execute-kbd-macro (vector (list 'mouse-1 position))))
  (redisplay t)
  (align-diff-integration--settle))

(defun align-diff-integration--pad (session side)
  "Return the one pad in SESSION's SIDE buffer."
  (with-current-buffer (align-buffer-buffer session side)
    (seq-find (lambda (overlay) (overlay-get overlay 'align-buffer-pad-rows))
              (overlays-in (point-min) (point-max)))))

(defun align-diff-integration--stand (session side line)
  "Select SESSION's SIDE window with point on LINE, and let it settle."
  (let* ((buffer (align-buffer-buffer session side))
         (window (get-buffer-window buffer)))
    (select-window window)
    (with-current-buffer buffer
      (align-buffer--walk-away)
      (goto-char (align-buffer-line-position buffer line))
      (set-window-point window (point)))
    (align-buffer-sync window)
    (align-diff-integration--settle)))

(defun align-diff-integration--walked (session side)
  "Return the row of SESSION's SIDE pad the cursor is on, or nil."
  (let ((walked (buffer-local-value 'align-buffer--walked
                                    (align-buffer-buffer session side))))
    (and walked (cdr walked))))

(defun align-diff-integration--top-row (session side)
  "Return the row of SESSION's SIDE pad the top of its window falls on, or nil."
  (let* ((window (get-buffer-window (align-buffer-buffer session side)))
         (pad (align-buffer--pad-at-top window)))
    (and pad (align-buffer--pad-row-at window pad (car (align-buffer--band window))))))

(defun align-diff-integration--out-by (session side)
  "Return how far out of level the walked row and its own line are drawn.
Nil when no pad is being walked, which is a different answer from zero."
  (let* ((buffer (align-buffer-buffer session side))
         (window (get-buffer-window buffer))
         (walked (buffer-local-value 'align-buffer--walked buffer)))
    (when walked
      (pcase-let* ((`(,overlay . ,row) walked)
                   (facing (overlay-get overlay 'align-buffer-pad-facing))
                   (line (align-buffer-pad-facing-line overlay row))
                   (other (get-buffer-window facing))
                   (wanted (+ (align-buffer--pad-top window overlay)
                              (align-buffer--pad-above window overlay row)))
                   (drawn (+ (align-buffer--offset-in
                              other (align-buffer-line-position facing line))
                             (* (- row (align-buffer-pad-text-row overlay line))
                                (align-buffer--row-height other)))))
        (- drawn wanted)))))

(defun align-diff-integration--pair-out-by (session side)
  "Return how far out of level the pair of SIDE's own line is drawn.
Nil where the correspondence says nothing about that line, which is align-
buffer's own rule for leaving the panes alone."
  (let* ((buffer (align-buffer-buffer session side))
         (window (get-buffer-window buffer))
         (line (align-buffer-line-at buffer (window-point window)))
         (anchor (and line (align-buffer-anchor session side line))))
    (when (and anchor (<= (car anchor) line))
      (let* ((other-buffer (align-buffer-buffer
                            session (align-buffer-other-side side)))
             (other (get-buffer-window other-buffer))
             (own (align-buffer--offset-in
                   window (align-buffer-line-position buffer (car anchor)))))
        (when (and other (>= own 0))
          (- (align-buffer--offset-in
              other (align-buffer-line-position other-buffer (cdr anchor)))
             own))))))

(defun align-diff-integration--quit (session)
  "Take SESSION apart."
  (let ((align-diff-idle-delay nil))
    (align-buffer-quit session)))


;;; The scenarios

(align-diff-integration-defscenario a-pad-stands-for-the-added-lines
  (let ((session (align-diff-integration--session)))
    (align-diff-integration--expect
     "the pad's rows" align-diff-integration--added
     (align-buffer-pad-rows (align-diff-integration--pad session 'left)))
    (align-diff-integration--expect
     "no pad on the side that has the lines" nil
     (align-diff-integration--pad session 'right))
    (align-diff-integration--quit session)))

(align-diff-integration-defscenario walking-into-a-pad
  ;; Three presses from two lines above: one onto the line above the gap, then
  ;; into the first and second rows of the pad.
  (let ((session (align-diff-integration--session)))
    (align-diff-integration--stand session 'left
                                   (- align-diff-integration--at 2))
    (align-diff-integration--keys "<down>" 3)
    (align-diff-integration--expect "the row walked to" 1
                                    (align-diff-integration--walked session
                                                                    'left))
    (align-diff-integration--expect "out of level by" 0
                                    (align-diff-integration--out-by session
                                                                    'left))
    (align-diff-integration--quit session)))

(align-diff-integration-defscenario walking-a-pad-leaves-point-alone
  (let ((session (align-diff-integration--session)))
    (align-diff-integration--stand session 'left
                                   (- align-diff-integration--at 1))
    (align-diff-integration--keys "<down>" 4)
    ;; The line the rows hang after, which is where point waits for as long as
    ;; one of them is read: that is the place they are drawn at, and a row
    ;; carries the cursor only while point is there.
    (align-diff-integration--expect
     "point's line" (1- align-diff-integration--at)
     (align-buffer-line-at (align-buffer-buffer session 'left)
                           (point)))
    (align-diff-integration--expect "the row walked to" 3
                                    (align-diff-integration--walked session
                                                                    'left))
    (align-diff-integration--quit session)))

(align-diff-integration-defscenario walking-the-whole-pad-and-out-of-it
  (let ((session (align-diff-integration--session)))
    (align-diff-integration--stand session 'left
                                   (- align-diff-integration--at 1))
    ;; Every row of the pad, and one press more to leave it.
    (align-diff-integration--keys "<down>" (1+ align-diff-integration--added))
    (align-diff-integration--expect "nothing walked any more" nil
                                    (align-diff-integration--walked session
                                                                    'left))
    (align-diff-integration--expect
     "point's line" align-diff-integration--at
     (align-buffer-line-at (align-buffer-buffer session 'left) (point)))
    (align-diff-integration--quit session)))

(align-diff-integration-defscenario walking-a-pad-stays-level-throughout
  (let ((session (align-diff-integration--session))
        (out nil))
    (align-diff-integration--stand session 'left
                                   (- align-diff-integration--at 1))
    (dotimes (_ align-diff-integration--added)
      (align-diff-integration--keys "<down>")
      (let ((now (align-diff-integration--out-by session 'left)))
        (when (and now (not (zerop now))) (push now out))))
    (align-diff-integration--expect "rows drawn out of level" nil
                                    (nreverse out))
    (align-diff-integration--quit session)))

(align-diff-integration-defscenario walking-back-up-a-pad
  (let ((session (align-diff-integration--session)))
    ;; From the line the pad hangs on, upwards into its last row.
    (align-diff-integration--stand session 'left align-diff-integration--at)
    (align-diff-integration--keys "<up>" 2)
    (align-diff-integration--expect
     "the row walked to" (- align-diff-integration--added 2)
     (align-diff-integration--walked session 'left))
    (align-diff-integration--expect "out of level by" 0
                                    (align-diff-integration--out-by session
                                                                    'left))
    (align-diff-integration--quit session)))

(align-diff-integration-defscenario the-wheel-crawls-through-a-pad
  (let* ((session (align-diff-integration--session))
         (window (get-buffer-window (align-buffer-buffer session 'left)))
         (notches 0)
         (entered nil)
         (after nil))
    (align-diff-integration--stand session 'left
                                   (- align-diff-integration--at 1))
    ;; Notch by notch until a row of the pad is what is being read.  An
    ;; ordinary scroll crosses the whole pad in one step and never reads one.
    (while (and (< notches 40)
                (not (setq entered (align-diff-integration--walked session
                                                                   'left))))
      (align-diff-integration--wheel window 'wheel-down)
      (setq notches (1+ notches)))
    (align-diff-integration--expect "a row of the pad is being read" t
                                    (integerp entered))
    (align-diff-integration--expect "out of level by" 0
                                    (align-diff-integration--out-by session
                                                                    'left))
    ;; And it goes on through the pad rather than over it: five notches of
    ;; three rows each move fifteen rows down it.
    (align-diff-integration--wheel window 'wheel-down 5)
    (setq after (align-diff-integration--walked session 'left))
    (align-diff-integration--expect "rows crossed by five notches"
                                    (* 5 align-buffer-wheel-rows)
                                    (and after entered (- after entered)))
    (align-diff-integration--expect "still out of level by" 0
                                    (align-diff-integration--out-by session
                                                                    'left))
    (align-diff-integration--quit session)))

(align-diff-integration-defscenario a-page-into-a-pad-reads-one-of-its-rows
  (let ((session (align-diff-integration--session)))
    (align-diff-integration--stand session 'left
                                   (- align-diff-integration--at 1))
    (align-diff-integration--keys "C-v")
    (align-diff-integration--expect
     "a row of the pad is being read" t
     (integerp (align-diff-integration--walked session 'left)))
    (align-diff-integration--expect "out of level by" 0
                                    (align-diff-integration--out-by session
                                                                    'left))
    (align-diff-integration--quit session)))

(align-diff-integration-defscenario a-page-up-into-a-pad-reads-one-of-its-rows
  ;; Paged up until the pad is reached rather than once: how many lines stand
  ;; between the reader and the pad is the shape of the fixture, and what the
  ;; scenario is about is the press that arrives.
  (let ((session (align-diff-integration--session))
        (pages 0))
    (align-diff-integration--stand session 'left
                                   align-diff-integration--below)
    (while (and (< pages 6)
                (not (integerp (align-diff-integration--walked session 'left))))
      (align-diff-integration--keys "M-v")
      (setq pages (1+ pages)))
    (align-diff-integration--expect
     "a row of the pad is being read" t
     (integerp (align-diff-integration--walked session 'left)))
    (align-diff-integration--expect "out of level by" 0
                                    (align-diff-integration--out-by session
                                                                    'left))
    (align-diff-integration--quit session)))

(align-diff-integration-defscenario the-wheel-crawls-back-up-through-a-pad
  ;; Read at the window's top rather than at the cursor: going up, the row the
  ;; cursor is on drifts down the screen and stays where it is until it would
  ;; fall off, and it is the view that the wheel moves.
  (let* ((session (align-diff-integration--session))
         (window (get-buffer-window (align-buffer-buffer session 'left)))
         (notches 0)
         (entered nil)
         (after nil))
    (align-diff-integration--stand session 'left
                                   align-diff-integration--below)
    (while (and (< notches 40)
                (not (setq entered (align-diff-integration--top-row session
                                                                    'left))))
      (align-diff-integration--wheel window 'wheel-up)
      (setq notches (1+ notches)))
    (align-diff-integration--expect "the top is inside the pad" t
                                    (integerp entered))
    (align-diff-integration--expect "a row of the pad is being read" t
                                    (integerp
                                     (align-diff-integration--walked session
                                                                     'left)))
    (align-diff-integration--wheel window 'wheel-up 5)
    (setq after (align-diff-integration--top-row session 'left))
    (align-diff-integration--expect "rows crossed back by five notches"
                                    (* 5 align-buffer-wheel-rows)
                                    (and after entered (- entered after)))
    (align-diff-integration--expect "out of level by" 0
                                    (align-diff-integration--out-by session
                                                                    'left))
    (align-diff-integration--quit session)))

(align-diff-integration-defscenario anything-else-gives-the-cursor-back
  (let ((session (align-diff-integration--session)))
    (align-diff-integration--stand session 'left
                                   (- align-diff-integration--at 1))
    (align-diff-integration--keys "<down>" 3)
    (align-diff-integration--expect
     "a row is being read" t
     (integerp (align-diff-integration--walked session 'left)))
    (align-diff-integration--keys "C-f")
    (align-diff-integration--expect "nothing walked any more" nil
                                    (align-diff-integration--walked session
                                                                    'left))
    (align-diff-integration--expect "the pairs have the windows back" nil
                                    align-buffer-inhibit-sync)
    (align-diff-integration--quit session)))

(align-diff-integration-defscenario a-pad-leaves-the-text-untouched
  (let ((session (align-diff-integration--session)))
    (align-diff-integration--stand session 'left
                                   (- align-diff-integration--at 1))
    (align-diff-integration--keys "<down>" 5)
    (align-diff-integration--expect
     "the old side's lines" 120
     (align-diff--line-count (align-buffer-buffer session 'left)))
    (align-diff-integration--expect
     "anything modified" nil
     (buffer-modified-p (align-buffer-buffer session 'left)))
    (align-diff-integration--quit session)))


;;; The same pad on the other side, where the run is a deletion
;;
;; A pad in the new pane faces the old one, so the walk scrolls the other way
;; and places the other window.  Every reading below is the mirror of one above.

(align-diff-integration-defscenario a-deletion-pads-the-new-side
  (let ((session (align-diff-integration--session 'right)))
    (align-diff-integration--expect
     "the pad's rows" align-diff-integration--added
     (align-buffer-pad-rows (align-diff-integration--pad session 'right)))
    (align-diff-integration--expect
     "no pad on the side that has the lines" nil
     (align-diff-integration--pad session 'left))
    (align-diff-integration--quit session)))

(align-diff-integration-defscenario walking-into-a-pad-on-the-new-side
  (let ((session (align-diff-integration--session 'right)))
    (align-diff-integration--stand session 'right
                                   (- align-diff-integration--at 2))
    (align-diff-integration--keys "<down>" 3)
    (align-diff-integration--expect "the row walked to" 1
                                    (align-diff-integration--walked session
                                                                    'right))
    (align-diff-integration--expect "out of level by" 0
                                    (align-diff-integration--out-by session
                                                                    'right))
    (align-diff-integration--quit session)))

(align-diff-integration-defscenario walking-a-pad-on-the-new-side-stays-level
  (let ((session (align-diff-integration--session 'right))
        (out nil))
    (align-diff-integration--stand session 'right
                                   (- align-diff-integration--at 1))
    (dotimes (_ align-diff-integration--added)
      (align-diff-integration--keys "<down>")
      (let ((now (align-diff-integration--out-by session 'right)))
        (when (and now (not (zerop now))) (push now out))))
    (align-diff-integration--expect "rows drawn out of level" nil
                                    (nreverse out))
    (align-diff-integration--quit session)))

(align-diff-integration-defscenario walking-out-of-a-pad-on-the-new-side
  (let ((session (align-diff-integration--session 'right)))
    (align-diff-integration--stand session 'right
                                   (- align-diff-integration--at 1))
    (align-diff-integration--keys "<down>" (1+ align-diff-integration--added))
    (align-diff-integration--expect "nothing walked any more" nil
                                    (align-diff-integration--walked session
                                                                    'right))
    (align-diff-integration--expect
     "point's line" align-diff-integration--at
     (align-buffer-line-at (align-buffer-buffer session 'right) (point)))
    (align-diff-integration--expect "the pairs have the windows back" nil
                                    align-buffer-inhibit-sync)
    (align-diff-integration--quit session)))

(align-diff-integration-defscenario stepping-off-the-end-of-a-one-sided-run
  ;; The reader is on the real lines of a run the other side has not got, and
  ;; the row facing each of them is a row of the pad over there.  Stepping off
  ;; the last of them is where the pairs take over again, and the press has to
  ;; move one line like any other.
  (let* ((session (align-diff-integration--session 'right))
         (last (+ align-diff-integration--at
                  align-diff-integration--added -1)))
    (align-diff-integration--stand session 'left last)
    (align-diff-integration--keys "<down>")
    (align-diff-integration--expect
     "point's line" (1+ last)
     (align-buffer-line-at (align-buffer-buffer session 'left) (point)))
    ;; Measured on the pair, there being no pad under the cursor any more:
    ;; the pairs have taken the panes back, which is the whole of what the
    ;; press had to leave behind it.
    (align-diff-integration--expect
     "out of level by" 0
     (align-diff-integration--pair-out-by session 'left))
    (align-diff-integration--quit session)))

(align-diff-integration-defscenario the-wheel-crawls-through-the-new-side-pad
  (let* ((session (align-diff-integration--session 'right))
         (window (get-buffer-window (align-buffer-buffer session 'right)))
         (notches 0)
         (entered nil)
         (after nil))
    (align-diff-integration--stand session 'right
                                   (- align-diff-integration--at 1))
    (while (and (< notches 40)
                (not (setq entered (align-diff-integration--walked session
                                                                   'right))))
      (align-diff-integration--wheel window 'wheel-down)
      (setq notches (1+ notches)))
    (align-diff-integration--expect "a row of the pad is being read" t
                                    (integerp entered))
    (align-diff-integration--wheel window 'wheel-down 5)
    (setq after (align-diff-integration--walked session 'right))
    (align-diff-integration--expect "rows crossed by five notches"
                                    (* 5 align-buffer-wheel-rows)
                                    (and after entered (- after entered)))
    (align-diff-integration--expect "out of level by" 0
                                    (align-diff-integration--out-by session
                                                                    'right))
    (align-diff-integration--quit session)))


;;; Running them

(defun align-diff-integration-run ()
  "Run every scenario and return the failures."
  (setq align-diff-integration--failures nil)
  (dolist (scenario (reverse align-diff-integration--scenarios))
    (let ((align-diff-integration--scenario (car scenario)))
      (condition-case error
          (funcall (cdr scenario))
        (error (push (format "%s: raised %S" (car scenario) error)
                     align-diff-integration--failures)))))
  (nreverse align-diff-integration--failures))

(align-diff-integration-defscenario a-click-lands-on-the-row-it-was-sent-at
  ;; The rows of a pad hold no position, so a click reports the line they hang
  ;; on wherever it lands; the row the pointer was over is the one the reader
  ;; meant, and the cursor goes there.
  (let* ((session (align-diff-integration--session))
         (window (get-buffer-window (align-buffer-buffer session 'left)))
         (pad (align-diff-integration--pad session 'left))
         (height (align-buffer--row-height window))
         (row 7))
    (align-diff-integration--stand session 'left
                                   (1- align-diff-integration--at))
    (align-diff-integration--click
     window (+ (align-buffer--pad-top window pad) (* row height) (/ height 2)))
    (align-diff-integration--expect
     "the row clicked" row (align-diff-integration--walked session 'left))
    (align-diff-integration--expect
     "out of level by" 0 (align-diff-integration--out-by session 'left))
    (align-diff-integration--quit session)))

(align-diff-integration-defscenario a-click-on-the-text-leaves-the-pads-alone
  (let* ((session (align-diff-integration--session))
         (buffer (align-buffer-buffer session 'left))
         (window (get-buffer-window buffer))
         (height (align-buffer--row-height window)))
    (align-diff-integration--stand session 'left 2)
    ;; A line the window is drawing a few rows down, clicked in the middle of
    ;; its own row, so the answer does not depend on where the pairs have put
    ;; the view.
    (let* ((position (align-buffer--line-below window (* 3 height)))
           (drawn (align-buffer--drawn-at window position))
           (wanted (align-buffer-line-at buffer position)))
      (align-diff-integration--click window (+ drawn (/ height 2)))
      (align-diff-integration--expect
       "the row clicked" nil (align-diff-integration--walked session 'left))
      (align-diff-integration--expect
       "point's line" wanted (align-buffer-line-at buffer (point))))
    (align-diff-integration--quit session)))

(align-diff-integration-defscenario the-wheel-over-the-facing-pane-leads
  ;; A notch is sent where the pointer is, which may be the pane the reader is
  ;; not standing in.  That pane is the one the reader is looking at, so it
  ;; leads and the one they stand in comes level with it.
  (let* ((session (align-diff-integration--session))
         (facing (get-buffer-window (align-buffer-buffer session 'right)))
         (reader (get-buffer-window (align-buffer-buffer session 'left))))
    (align-diff-integration--stand session 'left 10)
    ;; The whole view, lift and all: a pane can follow by a lift alone and
    ;; keep the start it had.
    (let ((was (cons (window-start reader) (window-vscroll reader t))))
      (align-diff-integration--wheel facing 'wheel-down 4)
      (align-diff-integration--expect
       "the pane the reader stands in followed" t
       (not (equal was (cons (window-start reader)
                             (window-vscroll reader t))))))
    (align-diff-integration--expect
     "out of level by" 0
     (align-diff-integration--pair-out-by session 'right))
    (align-diff-integration--quit session)))

(align-diff-integration-defscenario a-start-inside-a-pad-measures-the-rows
  ;; A start can land on the one character a pad's rows hang on, there being
  ;; no row boundary inside the rows for `recenter' to take instead, and the
  ;; rows are then drawn from the top of the window.  A span measured from
  ;; there is measured as none of them, so where a line below them is drawn
  ;; has to be put back rather than taken from the measurement.
  (let* ((session (align-diff-integration--several))
         (buffer (align-buffer-buffer session 'left))
         (window (get-buffer-window buffer))
         (pad (align-diff-integration--pad session 'left))
         (below (align-buffer-pad-anchor pad)))
    (align-diff-integration--stand session 'left
                                   (align-buffer-line-at buffer below))
    ;; A few rows above the line the rows hang in front of, which are rows of
    ;; the pad: there is no row boundary among them to take as a start, so the
    ;; character they hang on is taken and the window is lifted through them.
    (recenter 3)
    (redisplay t)
    (align-diff-integration--expect
     "the start is the character the rows hang on" t
     (= (window-start window) (align-buffer-pad-hangs pad)))
    (align-diff-integration--expect
     "where the line below the rows is drawn"
     (cdr (posn-x-y (posn-at-point below window)))
     (align-buffer--offset-in window below))
    (align-diff-integration--expect
     "where the first of the rows is drawn"
     (align-buffer--row-height window)
     (align-buffer--pad-top window pad))
    (align-diff-integration--quit session)))


;;; Moving hunk to hunk, over a diff with several of them

;; The session above has one hunk, which says nothing about going from one to
;; the next.  These build a diff with three: a stretch both sides have and
;; write differently, a run the new side alone has, and a run the old side
;; alone has.  So each pane stops on its own lines at one of them and on the
;; rows standing for the other side's lines at another.

(defconst align-diff-integration--hunks 3
  "How many hunks the session with several of them has.")

(defun align-diff-integration--several ()
  "Show two buffers differing in three places, and return the session."
  (delete-other-windows)
  (let* ((shared (lambda (from to)
                   (cl-loop for line from from to to
                            collect (format "shared line %03d" line))))
         (run (lambda (what)
                (cl-loop for line from 1 to 8
                         collect (format "%s only %03d" what line))))
         (old (generate-new-buffer " *align-diff several old*"))
         (new (generate-new-buffer " *align-diff several new*")))
    (with-current-buffer old
      (insert (align-diff-integration--text
               (append (funcall shared 1 30)
                       '("written one way A" "written one way B")
                       (funcall shared 33 60)
                       (funcall shared 61 90)
                       (funcall run "the old side")
                       (funcall shared 91 120))))
      (set-buffer-modified-p nil))
    (with-current-buffer new
      (insert (align-diff-integration--text
               (append (funcall shared 1 30)
                       '("written the other way A" "written the other way B")
                       (funcall shared 33 60)
                       (funcall run "the new side")
                       (funcall shared 61 90)
                       (funcall shared 91 120))))
      (set-buffer-modified-p nil))
    (setq align-diff-gap-marker 'pad)
    (setq align-diff-refine-hunks nil)
    (setq align-diff-idle-delay nil)
    (setq align-buffer-exact-alignment t)
    (let ((session (align-diff-buffers old new
                                       (list :owned-buffers (list old new)))))
      (dolist (side '(left right))
        (with-current-buffer (align-buffer-buffer session side)
          (local-set-key (kbd "C-c n") #'align-buffer-next-section)
          (local-set-key (kbd "C-c p") #'align-buffer-previous-section)))
      (align-diff-integration--settle)
      session)))

(defvar align-diff-integration--uncentrable 0
  "How many stops were allowed for being too near the beginning of a buffer.")

(defun align-diff-integration--off-centre (session side section)
  "Return how far from centred SECTION is drawn in SESSION's SIDE.
What is centred is the section as that pane shows it: its own lines where it
has them, and the rows of the pad standing for them where it has none.
Measured against `align-buffer--centre-pixels', which is the promise, with
`posn-at-point' for where a line is actually drawn."
  (let* ((buffer (align-buffer-buffer session side))
         (window (get-buffer-window buffer))
         (walked (buffer-local-value 'align-buffer--walked buffer)))
    (if walked
        (let ((block (align-buffer-pad-height window (car walked))))
          (- (align-buffer--pad-top window (car walked))
             (align-buffer--centre-pixels-of window block)))
      (let* ((line (align-buffer-section-line section side))
             (position (align-buffer-line-position buffer line))
             (drawn (with-selected-window window
                      (when (>= position (window-start window))
                        (cdr (posn-x-y (posn-at-point position window))))))
             (wanted (align-buffer--centre-pixels
                      window (align-buffer-section-count section side))))
        (when drawn
          ;; A section too near the beginning of the buffer to be brought that
          ;; far down is as centred as it can be: the window has nothing above
          ;; it left to scroll into, which is the rule the fuzzer holds a
          ;; centring to as well.  Counted, so a scenario can say whether its
          ;; own stops were held to the centre itself or to this.
          (if (and (< drawn wanted)
                   (with-current-buffer buffer
                     (= (window-start window) (point-min))))
              (progn (cl-incf align-diff-integration--uncentrable) 0)
            (- drawn wanted)))))))

(defun align-diff-integration--stop (session side &optional section)
  "Return where the reader stands in SESSION's SIDE and how level it reads.
A row of a pad where one is being read, and a line of the text otherwise, with
how far out of level that row or that line's pair is drawn.  Given SECTION,
how far from centred that section is drawn as well."
  (let ((walked (align-diff-integration--walked session side)))
    (append
     (if walked
         (list :row walked
               :out-by (align-diff-integration--out-by session side))
       (list :line (align-buffer-line-at (align-buffer-buffer session side)
                                         (point))
             :out-by (align-diff-integration--pair-out-by session side)))
     (when section
       (list :off-centre (align-diff-integration--off-centre session side
                                                             section))))))

(defun align-diff-integration--stop-name (stop)
  "Return what STOP stands at: a line of the text, or a row of a pad."
  (or (plist-get stop :line) (list 'row (plist-get stop :row))))

(defun align-diff-integration--press-sections (session side keys &optional most)
  "Press KEYS in SESSION's SIDE until the move is refused, and return the stops.
A move with nowhere left to go signals, which is how the end is found.  MOST
stops the walk where no refusal comes, and defaults to eight."
  ;; Quiet first, and quiet throughout: a message takes a row from every
  ;; window on the frame while it is up, so a centre worked out with one up is
  ;; a row from the one measured after it has gone.
  (message nil)
  (redisplay t)
  (align-diff-integration--stand session side 1)
  (let ((sections (align-buffer-session-sections session))
        (stops nil)
        (more t))
    (while (and more (< (length stops) (or most 8)))
      (condition-case nil
          (let ((inhibit-message t))
            (align-diff-integration--keys keys)
            ;; Walked from the first line, so the stops come in the order
            ;; the sections are in, and each one is read against its own.
            (push (align-diff-integration--stop
                   session side (nth (length stops) sections))
                  stops))
        (user-error (setq more nil))))
    (nreverse stops)))

(align-diff-integration-defscenario every-hunk-of-a-diff-is-a-section
  (let ((session (align-diff-integration--several)))
    (align-diff-integration--expect
     "sections" align-diff-integration--hunks
     (length (align-buffer-session-sections session)))
    ;; One of them the old side has no lines of and one the new side has none
    ;; of, which is what gives each pane a stop on rows rather than on lines.
    (align-diff-integration--expect
     "sections with lines on one side only" 2
     (seq-count (lambda (section)
                  (or (zerop (align-buffer-section-count section 'left))
                      (zerop (align-buffer-section-count section 'right))))
                (align-buffer-session-sections session)))
    (align-diff-integration--quit session)))

(align-diff-integration-defscenario every-hunk-is-reached-and-reads-level
  (let* ((session (align-diff-integration--several))
         (stops (align-diff-integration--press-sections session 'left "C-c n")))
    (align-diff-integration--expect "hunks stopped at"
                                    align-diff-integration--hunks
                                    (length stops))
    ;; One of the three is a run the old side has not got, so the old pane
    ;; stops on the rows standing for it rather than on a line of its own.
    (align-diff-integration--expect
     "stops on a pad's rows" 1
     (seq-count (lambda (stop) (plist-get stop :row)) stops))
    (align-diff-integration--expect
     "stops drawn out of level" nil
     (seq-filter (lambda (stop)
                   (let ((out (plist-get stop :out-by)))
                     (and out (not (zerop out)))))
                 stops))
    (align-diff-integration--expect
     "stops not drawn centred" nil
     (seq-filter (lambda (stop)
                   (not (equal 0 (plist-get stop :off-centre))))
                 stops))
    (align-diff-integration--quit session)))

(align-diff-integration-defscenario going-back-retraces-the-hunks
  (let* ((session (align-diff-integration--several))
         (down (align-diff-integration--press-sections session 'right "C-c n"))
         (up nil))
    ;; From the last one, back up: the stops are the ones on the way down,
    ;; reversed and without the one being stood on.
    (let ((more t))
      (while (and more (< (length up) 8))
        (condition-case nil
            (progn (align-diff-integration--keys "C-c p")
                   (push (align-diff-integration--stop session 'right) up))
          (user-error (setq more nil)))))
    (align-diff-integration--expect
     "the way back"
     (cdr (reverse (mapcar #'align-diff-integration--stop-name down)))
     (mapcar #'align-diff-integration--stop-name (nreverse up)))
    (align-diff-integration--quit session)))

(align-diff-integration-defscenario arrows-walk-on-from-a-section-move
  ;; The two features over one another: a move to a hunk the pane has no lines
  ;; of leaves the reader on the first row of its pad, and the arrows walk the
  ;; rest of the rows from there.
  (let ((session (align-diff-integration--several)))
    (align-diff-integration--stand session 'left 1)
    (let ((rows nil))
      (while (and (null (align-diff-integration--walked session 'left))
                  (< (length rows) 8))
        (align-diff-integration--keys "C-c n")
        (push t rows)))
    (align-diff-integration--expect
     "the row the move landed on" 0
     (align-diff-integration--walked session 'left))
    (align-diff-integration--keys "<down>")
    (align-diff-integration--expect
     "the row after one arrow" 1
     (align-diff-integration--walked session 'left))
    (align-diff-integration--expect "out of level by" 0
                                    (align-diff-integration--out-by session
                                                                    'left))
    (align-diff-integration--keys "<up>")
    (align-diff-integration--expect
     "the row after going back up" 0
     (align-diff-integration--walked session 'left))
    (align-diff-integration--quit session)))

(align-diff-integration-defscenario the-last-hunk-hands-the-boundary-over
  (let ((session (align-diff-integration--several))
        (calls nil))
    ;; To the last hunk first, with nobody handling the boundary: a handler
    ;; that takes it leaves the command quiet, and there would then be nothing
    ;; to say the end had been reached.
    (align-diff-integration--press-sections session 'right "C-c n")
    (let ((align-buffer-section-boundary-functions
           (list (lambda (boundary remaining)
                   (push (cons (eq boundary session) remaining) calls)
                   t))))
      ;; Standing on the last hunk, the press after it is the one the handler
      ;; takes, and the reader is left where they were.
      (let ((was (align-diff-integration--stop session 'right)))
        (align-diff-integration--keys "C-c n")
        (align-diff-integration--expect "the handler was called" '((t . 1))
                                        calls)
        (align-diff-integration--expect "the reader stayed put" was
                                        (align-diff-integration--stop session
                                                                      'right))))
    (align-diff-integration--quit session)))


(defun align-diff-integration--demo ()
  "Return a session over the demo's own diff, with the section keys bound.
Thirteen hunks, two of them runs taller than a window, and a changed line
whose two versions wrap to a different number of rows."
  (setq align-diff-gap-marker 'pad)
  (setq align-diff-idle-delay nil)
  (let ((session (align-diff-demo)))
    (dolist (side '(left right))
      (with-current-buffer (align-buffer-buffer session side)
        (local-set-key (kbd "C-c n") #'align-buffer-next-section)
        (local-set-key (kbd "C-c p") #'align-buffer-previous-section)))
    (align-diff-integration--settle)
    session))

(defun align-diff-integration--quit-demo (session)
  "Take the demo SESSION apart and take its buffers with it."
  (let ((buffers (list (align-buffer-buffer session 'left)
                       (align-buffer-buffer session 'right))))
    (align-diff-integration--quit session)
    (mapc #'kill-buffer buffers)))

(align-diff-integration-defscenario every-hunk-of-the-demo-is-level
  ;; The whole feature over the fixture the reader looks at: hunks of every
  ;; shape, among them two runs taller than a window, which a pad of its own
  ;; height stands against.
  (let ((session (align-diff-integration--demo)))
    (dolist (side '(left right))
      (let ((stops (align-diff-integration--press-sections session side
                                                           "C-c n" 20)))
        (align-diff-integration--expect
         (format "hunks stopped at from the %s" side)
         (length (align-buffer-session-sections session))
         (length stops))
        ;; The runs one side alone has leave that pane on a pad's rows.
        (align-diff-integration--expect
         (format "stops on a pad's rows from the %s, at least one" side) t
         (> (seq-count (lambda (stop) (plist-get stop :row)) stops) 0))
        (align-diff-integration--expect
         (format "stops drawn out of level from the %s" side) nil
         (seq-filter (lambda (stop)
                       (let ((out (plist-get stop :out-by)))
                         (and out (not (zerop out)))))
                     stops))
        (align-diff-integration--expect
         (format "stops not drawn centred from the %s" side) nil
         (seq-filter (lambda (stop)
                       (not (equal 0 (plist-get stop :off-centre))))
                     stops))
        ;; A section the buffer cannot bring down to the centre is allowed
        ;; for, and there is at most one such section in a run: the first,
        ;; which has only the lines above it to be pushed down by.
        (align-diff-integration--expect
         (format "stops allowed rather than centred from the %s" side) t
         (<= align-diff-integration--uncentrable 2))
        ;; And back to the top from the last hunk, which is the longest jump
        ;; the session has in it.
        (align-diff-integration--keys "M-<")
        (align-diff-integration--expect
         (format "level again at the top of the %s" side) 0
         (plist-get (align-diff-integration--stop session side) :out-by))))
    (align-diff-integration--quit-demo session)))


(defun align-diff-integration--report-file ()
  "Return where to write the report."
  (or (getenv "ALIGN_DIFF_INTEGRATION_OUT")
      (expand-file-name "align-diff-integration.txt" temporary-file-directory)))

(defun align-diff-integration-batch ()
  "Run the scenarios, write the report, and exit with a status."
  (let ((file (align-diff-integration--report-file)))
    (run-with-timer
     300 nil
     (lambda ()
       (with-temp-file file (insert "TIMED OUT\n"))
       (kill-emacs 2)))
    (run-with-idle-timer
     0.5 nil
     (lambda ()
       (align-buffer-background-frame)
       (let* ((failures (align-diff-integration-run))
              (lines (cons (format "%d scenarios, %d failure(s)"
                                   (length align-diff-integration--scenarios)
                                   (length failures))
                           failures)))
         (with-temp-file file
           (dolist (line lines) (insert line "\n")))
         (kill-emacs (if failures 1 0)))))))

(provide 'align-diff-integration)

;;; align-diff-integration.el ends here
