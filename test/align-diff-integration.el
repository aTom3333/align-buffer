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
;;   emacs -Q -L . -L test -l test/align-diff-integration.el \
;;         -f align-diff-integration-batch

;;; Code:

(require 'align-diff)
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
    (seq-find (lambda (overlay) (overlay-get overlay 'align-diff-pad-rows))
              (overlays-in (point-min) (point-max)))))

(defun align-diff-integration--stand (session side line)
  "Select SESSION's SIDE window with point on LINE, and let it settle."
  (let* ((buffer (align-buffer-buffer session side))
         (window (get-buffer-window buffer)))
    (select-window window)
    (with-current-buffer buffer
      (align-diff--walk-away)
      (goto-char (align-buffer-line-position buffer line))
      (set-window-point window (point)))
    (align-buffer-sync window)
    (align-diff-integration--settle)))

(defun align-diff-integration--walked (session side)
  "Return the row of SESSION's SIDE pad the cursor is on, or nil."
  (let ((walked (buffer-local-value 'align-diff--walked
                                    (align-buffer-buffer session side))))
    (and walked (cdr walked))))

(defun align-diff-integration--top-row (session side)
  "Return the row of SESSION's SIDE pad the top of its window falls on, or nil."
  (let* ((window (get-buffer-window (align-buffer-buffer session side)))
         (pad (align-diff--pad-at-top window)))
    (and pad (align-diff--row-at window pad (car (align-buffer--band window))))))

(defun align-diff-integration--out-by (session side)
  "Return how far out of level the walked row and its own line are drawn.
Nil when no pad is being walked, which is a different answer from zero."
  (let* ((buffer (align-buffer-buffer session side))
         (window (get-buffer-window buffer))
         (walked (buffer-local-value 'align-diff--walked buffer)))
    (when walked
      (pcase-let* ((`(,overlay . ,row) walked)
                   (facing (overlay-get overlay 'align-diff-pad-facing))
                   (first (overlay-get overlay 'align-diff-pad-facing-line))
                   (other (get-buffer-window facing))
                   (wanted (+ (align-diff--pad-top window overlay)
                              (* row (align-buffer--row-height window))))
                   (drawn (align-diff--offset-in
                           other (align-buffer-line-position facing
                                                             (+ first row)))))
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
             (own (align-diff--offset-in
                   window (align-buffer-line-position buffer (car anchor)))))
        (when (and other (>= own 0))
          (- (align-diff--offset-in
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
     (align-diff--pad-rows (align-diff-integration--pad session 'left)))
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
    (align-diff-integration--expect
     "point's line" align-diff-integration--at
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
                                    (* 5 align-diff-wheel-rows)
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
  (let ((session (align-diff-integration--session)))
    (align-diff-integration--stand session 'left
                                   align-diff-integration--below)
    (align-diff-integration--keys "M-v")
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
                                    (* 5 align-diff-wheel-rows)
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
     (align-diff--pad-rows (align-diff-integration--pad session 'right)))
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
                                    (* 5 align-diff-wheel-rows)
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
     window (+ (align-diff--pad-top window pad) (* row height) (/ height 2)))
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
       (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
       (redisplay t)
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
