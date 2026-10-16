;;; align-diff-sweep.el --- A whole buffer, one key at a time -*- lexical-binding: t; -*-

;;; Commentary:

;; One reader's test, done exhaustively: from the first line to the last with
;; the down arrow, then back to the first with the up arrow, checking at every
;; single press that
;;
;;   - the press moved exactly one place, to the next one there is, counting a
;;     pad's rows as places of their own,
;;   - the cursor is still drawn inside the window,
;;   - the two panes are drawn level on whatever governs the place we reached:
;;     the pad row being walked and the line it stands for, or the pair of the
;;     line point is on.
;;
;; The sweep runs in both panes.  Everything it reports is a place where a
;; reader holding the arrow key down would see something wrong.
;;
;; Needs a graphical frame.  Run with:
;;   emacs -Q --iconic -L . -L test -l test/align-diff-sweep.el \
;;         -f align-diff-sweep-batch
;;
;; The scroll keys are swept too: a page and a wheel notch, to the end of the
;; buffer and back, checking that the view moves exactly as far as the key
;; asked for and that the panes stay level all the way.
;;
;; The keys that are neither, from the places where a pad is in reach: the two
;; ends of the buffer, a jump to a line, a recentre, a step of a character, and
;; a count given to an arrow.
;;
;; ALIGN_DIFF_SWEEP_SIDE limits it to one pane, ALIGN_DIFF_SWEEP_FROM and
;; ALIGN_DIFF_SWEEP_TO to a range of lines, which is how one break found here
;; is looked at on its own.

;;; Code:

(require 'align-diff)
(require 'align-diff-demo)
(require 'align-buffer-background)
(require 'cl-lib)

;; Evil is swept where it is installed, and the sweep loads it itself.
(declare-function evil-local-mode "evil-core" (&optional arg))
(declare-function evil-normal-state "evil-states" (&optional arg))

(defvar align-diff-sweep--problems nil
  "What went wrong, newest first.")

(defvar align-diff-sweep--seen 0
  "How many presses have been checked.")

(defvar align-diff-sweep-report 30
  "How many problems to describe before only counting them.")

(defvar align-diff-sweep--held 1
  "How many states the windows held during the settle just done.")

(defvar align-diff-sweep--states nil
  "The states the windows held during the settle just done, in order.")

(defun align-diff-sweep--state-lines (states)
  "Return STATES as lines, each window's start, lift and point.
The same state coming round again is what a pane moving and moving back looks
like, and is marked."
  (let ((seen nil)
        (lines nil)
        (number 0))
    (dolist (state states)
      (setq number (1+ number))
      (push (format "    %d%s %s" number
                    (if (member state seen) " (back to an earlier one)" "")
                    (mapconcat (lambda (one)
                                 (format "%s/%s/%s" (nth 1 one) (nth 2 one)
                                         (nth 3 one)))
                               state "  "))
            lines)
      (push state seen))
    (nreverse lines)))

(defvar align-diff-sweep--flicker nil
  "How many presses held each number of states, as an alist of (STATES . PRESSES).")

(defvar align-diff-sweep--flickeriest nil
  "The presses that held more than two states, newest first.")

(defun align-diff-sweep--note-flicker (where)
  "Write down how much the screen moved after WHERE was pressed."
  (let ((entry (assq align-diff-sweep--held align-diff-sweep--flicker)))
    (if entry
        (setcdr entry (1+ (cdr entry)))
      (push (cons align-diff-sweep--held 1) align-diff-sweep--flicker)))
  (when (> align-diff-sweep--held 2)
    (push (string-join
           (cons (format "%s (%d states)" where align-diff-sweep--held)
                 (align-diff-sweep--state-lines align-diff-sweep--states))
           "
")
          align-diff-sweep--flickeriest)))


;;; Where we are, and where the keys should take us

(defun align-diff-sweep--pad-rows (buffer line)
  "Return how many rows the pad hanging above LINE in BUFFER has, or nil."
  (let ((position (align-buffer-line-position buffer line)))
    (when position
      (with-current-buffer buffer
        (let ((pad (seq-find (lambda (overlay)
                               (overlay-get overlay 'align-buffer-pad-rows))
                             (overlays-in position position))))
          (and pad (align-buffer-pad-rows pad)))))))

(defun align-diff-sweep--places (buffer from to)
  "Return every place the arrows should visit in BUFFER from line FROM to TO.
A place is (LINE . ROW), ROW being the row of the pad hanging above LINE, or
nil for the line's own text.  The pad above FROM is left out: a sweep starting
there starts on the line itself."
  (let ((places (list (cons from nil))))
    (cl-loop for line from (1+ from) to to
             do (let ((rows (align-diff-sweep--pad-rows buffer line)))
                  (dotimes (row (or rows 0))
                    (push (cons line row) places))
                  (push (cons line nil) places)))
    (nreverse places)))

(defun align-diff-sweep--place (buffer)
  "Return where BUFFER is being read, as (LINE . ROW)."
  (with-current-buffer buffer
    (cons (align-buffer-line-at buffer (point))
          (cdr align-buffer--walked))))

(defun align-diff-sweep--panes (session)
  "Return what both of SESSION's panes are showing, as a string."
  (string-join
   (mapcar (lambda (side)
             (let* ((buffer (align-buffer-buffer session side))
                    (window (get-buffer-window buffer)))
               (format "%s point %s start %s lift %s"
                       side
                       (align-buffer-line-at buffer (window-point window))
                       (align-buffer-line-at buffer (window-start window))
                       (window-vscroll window t))))
           '(left right))
   " | "))


;;; What the screen says about it

(defun align-diff-sweep--busy-p ()
  "Return non-nil while align-buffer still has something of its own queued."
  (or align-buffer--pending-timer
      align-buffer--pending-place
      align-buffer--wait-timer))

(defun align-diff-sweep--settle ()
  "Let the timers and the redisplay they ask for run, catching what they raise.
An error inside a timer or a redisplay is not one press going wrong, so it is
written down and the sweep carries on."
  (condition-case error (align-diff-sweep--settle-1)
    (error (align-diff-sweep--note 'settle-raised align-diff-sweep--seen nil
                                   (format "%S" error)))))

(defun align-diff-sweep--settle-1 ()
  "Let the timers and the redisplay they ask for run, until nothing is left.
Waiting for a round that moves nothing is not enough on its own: a placement
queued on a timer has moved nothing yet, and a round shorter than the timer
reads that as rest.  So we wait for the queue to empty as well, and sleep for
longer than the delay a lift waits.

The states the windows hold on the way are counted as they go by, that count
being the flicker a reader sees: one state is a press whose own effect was
the last word, two is a press the placement answered, and more than that is
the screen moving again after the reader could already see it."
  (let ((before nil)
        (rounds 0)
        (held (list (align-diff-sweep--where))))
    (while (and (< rounds 40)
                (or (align-diff-sweep--busy-p)
                    (not (equal before (align-diff-sweep--where)))))
      (setq before (align-diff-sweep--where))
      (redisplay t)
      (sit-for (+ align-buffer--lift-delay 0.01))
      (unless (equal (car held) (align-diff-sweep--where))
        (push (align-diff-sweep--where) held))
      (setq rounds (1+ rounds)))
    (setq align-diff-sweep--held (length held))
    (setq align-diff-sweep--states (nreverse held))))

(defun align-diff-sweep--where ()
  "Return where every window of the frame sits."
  (mapcar (lambda (window)
            (list window (window-start window) (window-vscroll window t)
                  (window-point window)))
          (window-list)))

(defun align-diff-sweep--cursor-shown-p (window)
  "Return non-nil when WINDOW draws its cursor inside itself.
A walked pad row carries the cursor, and the row is what is then asked about,
point being on a line the pad hangs below everything shown."
  (let* ((buffer (window-buffer window))
         (walked (buffer-local-value 'align-buffer--walked buffer)))
    (if walked
        (align-buffer--pad-row-shown-p window (car walked) (cdr walked))
      (let ((offset (align-buffer--offset-in window (window-point window))))
        (and (>= offset 0)
             (<= (+ offset (align-buffer--row-height window))
                 (window-body-height window t)))))))

(defun align-diff-sweep--out-by (session side)
  "Return how far out of level the two panes are drawn on SIDE's place.
Nil when nothing says what level means there: a line with no counterpart, or
a pair the leader is no longer showing, which is align-buffer's own rule."
  (let* ((buffer (align-buffer-buffer session side))
         (window (get-buffer-window buffer))
         (walked (buffer-local-value 'align-buffer--walked buffer)))
    (if walked
        (pcase-let* ((`(,overlay . ,row) walked)
                     (facing (overlay-get overlay 'align-buffer-pad-facing))
                     (first (overlay-get overlay 'align-buffer-pad-facing-line))
                     (other (and (buffer-live-p facing)
                                 (get-buffer-window facing))))
          (when (and other first)
            (let ((wanted (+ (align-buffer--pad-top window overlay)
                             (* row (align-buffer--row-height window))))
                  (drawn (align-buffer--offset-in
                          other (align-buffer-line-position facing
                                                            (+ first row)))))
              (- drawn wanted))))
      ;; Read from the window rather than from point: a press may have left
      ;; the reader in the other pane, and this one is still the side asked
      ;; about.
      (let* ((line (align-buffer-line-at buffer (window-point window)))
             (anchor (align-buffer-anchor session side line)))
        (when (and anchor (<= (car anchor) line))
          (let* ((other-buffer (align-buffer-buffer
                                session (align-buffer-other-side side)))
                 (other (get-buffer-window other-buffer))
                 (own (align-buffer--offset-in
                       window (align-buffer-line-position buffer
                                                          (car anchor)))))
            ;; Above the leader's own top there is no row to be level with,
            ;; and the follower is then held at the top on purpose.
            (when (and other (>= own 0))
              (- (align-buffer--offset-in
                  other (align-buffer-line-position other-buffer
                                                    (cdr anchor)))
                 own))))))))


;;; The sweep

(defun align-diff-sweep--note (what where place detail)
  "Remember that WHAT went wrong at WHERE, on PLACE, with DETAIL."
  (push (list what where place detail) align-diff-sweep--problems))

(defvar align-diff-sweep--read 1
  "How far through the message log we have read.")

(defconst align-diff-sweep--trouble
  (concat "rror\\|Wrong type\\|Args out of range\\|out of range"
          "\\|void-\\|Symbol.s \\|Invalid\\|Debugger\\|Wrong number")
  "What a message says when something has gone wrong behind our back.")

(defun align-diff-sweep--messages ()
  "Return what has been said in the message log since the last look.

A press is not the only thing that can raise an error: one raised in a timer,
in a hook, or inside redisplay is caught by Emacs and only said out loud, and
a hook that raises is taken off the hook into the bargain.  So we read what
was said, which is the one place all of them show up."
  (let ((buffer (get-buffer "*Messages*")))
    (when buffer
      (with-current-buffer buffer
        (let ((from (min align-diff-sweep--read (point-max))))
          (setq align-diff-sweep--read (point-max))
          (seq-filter
           (lambda (line)
             (string-match-p align-diff-sweep--trouble line))
           (split-string (buffer-substring-no-properties from (point-max))
                         "\n" t)))))))

(defun align-diff-sweep--check-quiet (where place)
  "Note anything said behind our back by WHERE, which reached PLACE."
  (dolist (line (align-diff-sweep--messages))
    (align-diff-sweep--note 'said where place line)))

(defun align-diff-sweep--pinned-at-end-p (window)
  "Return non-nil when WINDOW can be scrolled no further towards the end.
Its start is the last line of its buffer and the lift has brought that line's
own text to the top, which is as far as a scroll of ours goes."
  (let* ((buffer (and (window-live-p window) (window-buffer window)))
         (last (and buffer
                    (align-buffer-line-position
                     buffer (align-diff--line-count buffer)))))
    (and last
         ;; At or after it: a buffer ending in a newline has an empty line
         ;; after its last one, and a window starting there is as far on as
         ;; it can be.
         (>= (window-start window) last)
         (>= (window-vscroll window t)
             (align-buffer--hanging-height window (window-start window))))))

(defun align-diff-sweep--check (session side where place)
  "Check what the screen shows of SESSION's SIDE at WHERE, having reached PLACE."
  (let* ((buffer (align-buffer-buffer session side))
         (window (get-buffer-window buffer))
         (out (align-diff-sweep--out-by session side)))
    (unless (align-diff-sweep--cursor-shown-p window)
      (align-diff-sweep--note 'cursor-off-screen where place
                              (format "start %s lift %s point line %s"
                                      (align-buffer-line-at
                                       buffer (window-start window))
                                      (window-vscroll window t)
                                      (align-buffer-line-at
                                       buffer (window-point window)))))
    (when (and out (not (zerop out))
               ;; Unless one of the two has run out of buffer: a pane whose
               ;; last line is drawn at the top of it can be scrolled no
               ;; further, so where the other asks for more there is no level
               ;; to be had and nothing to put right.
               (not (align-diff-sweep--pinned-at-end-p window))
               (not (align-diff-sweep--pinned-at-end-p
                     (get-buffer-window
                      (align-buffer-buffer session
                                           (align-buffer-other-side side))))))
      (align-diff-sweep--note 'out-of-level where place
                              (format "%s px, %s" out
                                      (align-diff-sweep--panes session))))))

(defun align-diff-sweep--press (key)
  "Send KEY as real input, and return the error it raised, or nil."
  (condition-case error
      (progn (execute-kbd-macro (kbd key)) nil)
    (error error)))

(defun align-diff-sweep--rows-of-line (window position)
  "Return how many screen rows the text of POSITION's line takes in WINDOW.
The rows of a pad hanging on the line are not its text and are left out.  A
line wider than the window wraps onto a row of its own for each part, and the
line-moving commands move by one of those rows at a time, so crossing such a
line takes a press for each."
  (let* ((buffer (window-buffer window))
         (height (align-buffer--row-height window))
         (pad (align-buffer-pad-at
               (align-buffer-line-position
                buffer (align-buffer-line-at buffer position))))
         (rows (if pad (align-buffer-pad-rows pad) 0)))
    (max 1 (- (/ (align-buffer-line-display-height window position) height)
              rows))))

(defun align-diff-sweep--walk (session side key places)
  "Press KEY through PLACES in SESSION's SIDE, checking every press.
PLACES begins with where the sweep already is.

A press that lands somewhere unexpected is written down and then put right,
by standing on the place it should have reached.  So each press is a test of
its own and one break does not report every press after it."
  (let ((buffer (align-buffer-buffer session side))
        (step 0)
        (expected (cdr places)))
    (dolist (place expected)
      (setq step (1+ step))
      (let* ((window (get-buffer-window buffer))
             ;; A press moves one screen row, so a line that wraps takes one
             ;; press for each of its rows before the next place is reached.
             (allowed (align-diff-sweep--rows-of-line window (window-point
                                                              window)))
             (where (format "%s %s %s" side key step))
             (raised nil)
             (got nil)
             (tries 0))
        (while (and (progn
                      (setq align-diff-sweep--seen
                            (1+ align-diff-sweep--seen))
                      (setq raised (align-diff-sweep--press key))
                      (align-diff-sweep--settle)
                      (setq got (align-diff-sweep--place buffer))
                      (setq tries (1+ tries))
                      (not (equal place got)))
                    (not raised)
                    (< tries allowed)))
        (progn
          (cond
           (raised
            (align-diff-sweep--note 'raised where place (format "%S" raised)))
           ((not (equal place got))
            (align-diff-sweep--note 'wrong-place where place
                                    (format "got %S, %s" got
                                            (align-diff-sweep--panes session)))
            (if (cdr place)
                ;; A row of a pad cannot be stood on from outside, so from
                ;; there the sweep carries on from where it ended up.
                (setq place got)
              (align-diff-sweep--stand session side (car place))))
           (t nil))
          (align-diff-sweep--note-flicker where)
          (align-diff-sweep--check-quiet where place)
          (condition-case error
              (align-diff-sweep--check session side where place)
            (error (align-diff-sweep--note 'check-raised where place
                                           (format "%S" error)))))))))

(defun align-diff-sweep--end-shown-p (window)
  "Return non-nil when WINDOW is drawing the last line of its buffer."
  (let* ((buffer (window-buffer window))
         (last (align-buffer-line-position
                buffer (align-diff--line-count buffer))))
    (and last (>= last (window-start window))
         (< (align-buffer--offset-in window last)
            (window-body-height window t)))))

(defun align-diff-sweep--scroll (session side key rows)
  "Press KEY through SESSION's SIDE while it moves the view, checking each press.
ROWS is how many screen rows one press asks for, negative towards the
beginning of the buffer.  A press has to move the view that far and no
further: a view that travels further has jumped, which is what a recentering
redisplay does and what a reader sees as the text leaping away.

Measured by how far one position moves on the screen, that being the one
measurement the window's own lift cancels out of."
  (let* ((buffer (align-buffer-buffer session side))
         (window (get-buffer-window buffer))
         (wanted (* rows (align-buffer--row-height window)))
         (step 0))
    (while (let* ((mark (window-start window))
                  (before (align-buffer--offset-in window mark))
                  (where (format "%s %s %s" side key (setq step (1+ step))))
                  (raised (progn (setq align-diff-sweep--seen
                                       (1+ align-diff-sweep--seen))
                                 (align-diff-sweep--press key))))
             (align-diff-sweep--settle)
             (let* ((travelled (- before (align-buffer--offset-in window mark))))
               (when raised
                 (align-diff-sweep--note 'raised where nil
                                         (format "%S" raised)))
               (when (and (if (> wanted 0)
                              (> travelled wanted)
                            (< travelled wanted))
                          ;; Not where the end of the buffer is in sight: a
                          ;; window holding fewer lines than the scroll
                          ;; margin has nowhere to put point outside it, and
                          ;; redisplay then starts the window on point's own
                          ;; line.  The view arrives at the end of the buffer
                          ;; a row or two early, which is where it was going.
                          (not (align-diff-sweep--end-shown-p window)))
                 (align-diff-sweep--note 'view-jumped where nil
                                         (format "asked %s px, moved %s px"
                                                 wanted travelled)))
               (align-diff-sweep--note-flicker where)
               (align-diff-sweep--check-quiet where nil)
               (condition-case error
                   (align-diff-sweep--check session side where nil)
                 (error (align-diff-sweep--note 'check-raised where nil
                                                (format "%S" error))))
               ;; Carried on while the view is still moving: the far end of
               ;; the buffer is where a scroll stops, and that is the end of
               ;; the sweep rather than a press going wrong.
               (and (not raised) (not (zerop travelled))
                    (< step 400)))))
    step))

(defun align-diff-sweep--scroll-side (session side)
  "Scroll SESSION's SIDE from end to end of the buffer, by every key there is."
  (let* ((buffer (align-buffer-buffer session side))
         (window (get-buffer-window buffer))
         (page (max 1 (- (window-body-height window)
                         next-screen-context-lines)))
         (last (align-diff--line-count buffer)))
    (dolist (key (list (cons "C-v" page)
                       (cons "<wheel-down>" align-buffer-wheel-rows)))
      (align-diff-sweep--stand session side 1)
      (align-diff-sweep--scroll session side (car key) (cdr key)))
    (dolist (key (list (cons "M-v" (- page))
                       (cons "<wheel-up>" (- align-buffer-wheel-rows))))
      (align-diff-sweep--stand session side last)
      (align-diff-sweep--scroll session side (car key) (cdr key)))))

(defvar align-diff-sweep--commands
  '(("M->" . nil) ("M-<" . nil) ("C-f" . nil) ("C-b" . nil) ("C-e" . nil)
    ("C-l" . nil) ("M-g M-g 150 RET" . nil) ("M-r" . nil)
    ("C-s samples RET" . nil) ("C-M-v" . nil) ("C-x o" . nil)
    ("C-u 5 <down>" . 5) ("C-u 5 <up>" . -5))
  "Keys that are neither a plain line move nor a scroll of the view.
Each with how many places it should move where that is known, and nil where
only the state it leaves behind can be judged.")

(defun align-diff-sweep--pad-lines (buffer)
  "Return the lines of BUFFER worth pressing an odd key on.
The line each pad hangs on and its neighbours, where a press has a pad to
cross or to come out of, and the two ends of the buffer."
  (let ((count (align-diff--line-count buffer))
        (lines nil))
    (dotimes (index count)
      (let ((line (1+ index)))
        (when (align-diff-sweep--pad-rows buffer line)
          (dolist (one (list (max 1 (1- line)) line (min count (1+ line))))
            (unless (memq one lines) (push one lines))))))
    (dolist (one (list 1 count))
      (unless (memq one lines) (push one lines)))
    (sort lines #'<)))

(defun align-diff-sweep--commands-side (session side)
  "Press every odd key there is in SESSION's SIDE, from the places that matter."
  (let* ((buffer (align-buffer-buffer session side))
         (range (align-diff-sweep--range buffer))
         (places (align-diff-sweep--places buffer (car range) (cdr range))))
    (dolist (line (align-diff-sweep--pad-lines buffer))
      (dolist (command align-diff-sweep--commands)
        (align-diff-sweep--stand session side line)
        (let* ((key (car command))
               (where (format "%s %s on line %s" side key line))
               (from (align-diff-sweep--place buffer))
               (raised (progn
                         (setq align-diff-sweep--seen
                               (1+ align-diff-sweep--seen))
                         (align-diff-sweep--press key))))
          (align-diff-sweep--settle)
          ;; The edges of the buffer are where these keys are pressed on
          ;; purpose, and a motion that runs into one says so by signalling,
          ;; which is what it should do.  A search that finds nothing below
          ;; where it started rings the bell, which ends the macro, and says
          ;; the same thing.
          (when (and raised
                     (not (memq (car raised) '(beginning-of-buffer
                                               end-of-buffer)))
                     (not (and (eq (car raised) 'user-error)
                               (string-match-p "ringing the bell"
                                               (or (cadr raised) "")))))
            (align-diff-sweep--note 'raised where nil (format "%S" raised)))
          (when (and (cdr command) (not raised))
            (let* ((index (cl-position from places :test #'equal))
                   (wanted (and index (nth (max 0 (+ index (cdr command)))
                                           places)))
                   (got (align-diff-sweep--place buffer)))
              (when (and wanted (not (equal wanted got)))
                (align-diff-sweep--note 'wrong-place where wanted
                                        (format "got %S" got)))))
          (align-diff-sweep--note-flicker where)
          (align-diff-sweep--check-quiet where nil)
          (condition-case error
              (align-diff-sweep--check session side where nil)
            (error (align-diff-sweep--note 'check-raised where nil
                                           (format "%S" error)))))))))

(defconst align-diff-sweep--evil-commands
  '(("C-d" . nil) ("C-u" . nil) ("G" . nil) ("g g" . nil) ("}" . nil)
    ("{" . nil) ("5 j" . 5) ("5 k" . -5))
  "Evil keys that are not a plain line move, with the places they should move.")

(defun align-diff-sweep--evil-side (session side)
  "Sweep SESSION's SIDE with evil's own keys, where evil is to be had.

What makes the pads behave for evil is that its line motions come through
`line-move' and its scrolls through `scroll-up' and `scroll-down', so these
presses are the check of that claim rather than a reading of its source."
  (let* ((buffer (align-buffer-buffer session side))
         (range (align-diff-sweep--range buffer))
         (places (align-diff-sweep--places buffer (car range) (cdr range)))
         (window (get-buffer-window buffer))
         (page (max 1 (- (window-body-height window)
                         next-screen-context-lines))))
    (dolist (one '(left right))
      (with-current-buffer (align-buffer-buffer session one)
        (evil-local-mode 1)
        (evil-normal-state)))
    ;; A line at a time, down and back up, through every pad on the way.
    (align-diff-sweep--stand session side (car range))
    (align-diff-sweep--walk session side "j" places)
    (align-diff-sweep--stand session side (cdr range))
    (align-diff-sweep--walk session side "k" (reverse places))
    ;; A page at a time, which evil asks for through `scroll-up'.
    (align-diff-sweep--stand session side 1)
    (align-diff-sweep--scroll session side "C-f" page)
    (align-diff-sweep--stand session side (cdr range))
    (align-diff-sweep--scroll session side "C-b" (- page))
    ;; And the rest, judged by what they leave behind them.
    (let ((align-diff-sweep--commands align-diff-sweep--evil-commands))
      (align-diff-sweep--commands-side session side))
    (dolist (one '(left right))
      (with-current-buffer (align-buffer-buffer session one)
        (evil-local-mode -1)))))

(defun align-diff-sweep--stand (session side line)
  "Select SESSION's SIDE window with point on LINE, and let it settle."
  (let* ((buffer (align-buffer-buffer session side))
         (window (get-buffer-window buffer)))
    (select-window window)
    (align-buffer--walk-away)
    (with-current-buffer buffer
      (goto-char (align-buffer-line-position buffer line))
      (set-window-point window (point)))
    (align-buffer-sync window)
    ;; As a command of the reader's would leave it: a point put here from
    ;; outside has had none of the keeping that every command does, and a pane
    ;; left with its own point behind a lift is not a state any press reaches.
    (redisplay t)
    (align-buffer--keep-point-in-band window)
    (align-diff-sweep--settle)))

(defun align-diff-sweep--session ()
  "Show the demo with its hunks padded, and return the session."
  (setq align-diff-gap-marker 'pad)
  (setq align-diff-idle-delay nil)
  ;; ALIGN_DIFF_SWEEP_EXACT=no runs the sweep with align-buffer's own exact
  ;; placement off, which is how a session with pads behaves for a reader who
  ;; has never heard of the setting.
  (setq align-buffer-exact-alignment
        (not (equal (getenv "ALIGN_DIFF_SWEEP_EXACT") "no")))
  ;; ALIGN_DIFF_SWEEP_PIXEL=yes runs it for a reader who has
  ;; `pixel-scroll-precision-mode' on, whose own wheel keys are bound where
  ;; they could take the notches from ours.
  (when (equal (getenv "ALIGN_DIFF_SWEEP_PIXEL") "yes")
    (pixel-scroll-precision-mode 1))
  (align-diff-demo))

(defun align-diff-sweep--range (buffer)
  "Return the first and last line to sweep in BUFFER."
  (let ((count (align-diff--line-count buffer))
        (from (getenv "ALIGN_DIFF_SWEEP_FROM"))
        (to (getenv "ALIGN_DIFF_SWEEP_TO")))
    (cons (if from (string-to-number from) 1)
          (min count (if to (string-to-number to) count)))))

(defconst align-diff-sweep--line-keys
  '(("<down>" . "<up>") ("C-n" . "C-p"))
  "The keys to walk a line with, as (DOWN . UP).
Several, because what carries a line move to the pads is `line-move' rather
than the name of a command, and these reach it by different roads.")

(defun align-diff-sweep--side (session side)
  "Sweep SESSION's SIDE down and back up, with every key that moves a line."
  (let* ((buffer (align-buffer-buffer session side))
         (range (align-diff-sweep--range buffer))
         (places (align-diff-sweep--places buffer (car range) (cdr range))))
    (dolist (keys align-diff-sweep--line-keys)
      (align-diff-sweep--stand session side (car range))
      (align-diff-sweep--walk session side (car keys) places)
      ;; Stood again at the far end rather than carried on from wherever the
      ;; way down ended: the two directions are then measured on their own,
      ;; and one direction's break does not report every press of the other.
      (align-diff-sweep--stand session side (cdr range))
      (align-diff-sweep--walk session side (cdr keys) (reverse places)))
    (length places)))

(defun align-diff-sweep-run ()
  "Sweep both panes and return the problems, oldest first."
  (setq align-diff-sweep--problems nil)
  (setq align-diff-sweep--seen 0)
  (setq align-diff-sweep--flicker nil)
  (setq align-diff-sweep--flickeriest nil)
  (setq align-diff-sweep--read 1)
  ;; No debugger and no beep: an error is something to write down and carry on
  ;; from, and a debugger window would leave the sweep standing there.
  (setq debug-on-error nil)
  (setq command-error-function
        (lambda (error _context _caller)
          (align-diff-sweep--note 'command-error align-diff-sweep--seen nil
                                  (format "%S" error))))
  (let ((session (condition-case error (align-diff-sweep--session)
                   (error (align-diff-sweep--note 'no-session 0 nil
                                                  (format "%S" error))
                          nil)))
        (sides (let ((one (getenv "ALIGN_DIFF_SWEEP_SIDE")))
                 (if one (list (intern one)) '(left right)))))
    (when session
      (unwind-protect
          (dolist (side sides)
            (dolist (sweep (append '(align-diff-sweep--side
                                     align-diff-sweep--scroll-side
                                     align-diff-sweep--commands-side)
                                   ;; Evil only where it is installed and
                                   ;; asked for: its keys are a reader's as
                                   ;; much as Emacs's own, and nothing else
                                   ;; says whether they reach the pads.
                                   (and (getenv "ALIGN_DIFF_SWEEP_EVIL")
                                        (require 'evil nil t)
                                        '(align-diff-sweep--evil-side))))
              (condition-case error
                  (funcall sweep session side)
                (error (align-diff-sweep--note 'side-raised
                                               align-diff-sweep--seen
                                               (format "%s %s" sweep side)
                                               (format "%S" error))))))
        (align-diff-sweep--check-quiet align-diff-sweep--seen nil)
        (let ((align-diff-idle-delay nil))
          (ignore-errors (align-buffer-quit session))))))
  (nreverse align-diff-sweep--problems))

(defun align-diff-sweep--lines (problems)
  "Return the report on PROBLEMS, as a list of lines."
  (let ((counts nil))
    (dolist (problem problems)
      (let ((cell (assq (car problem) counts)))
        (if cell (setcdr cell (1+ (cdr cell)))
          (push (cons (car problem) 1) counts))))
    (append
     (list (format "%d presses checked, %d problem(s)"
                   align-diff-sweep--seen (length problems)))
     (mapcar (lambda (count) (format "  %s: %d" (car count) (cdr count)))
             (nreverse counts))
     ;; Counted and reported, not judged: two states is a press and the
     ;; placement answering it, and what more than that means depends on
     ;; which press it was.
     (list (format "states held per press: %s%s"
                   (mapconcat (lambda (one) (format "%d by %d presses"
                                                    (car one) (cdr one)))
                              (sort (copy-sequence align-diff-sweep--flicker)
                                    (lambda (a b) (< (car a) (car b))))
                              ", ")
                   ""))
     (mapcar (lambda (one) (format "  moved twice over: %s" one))
             (seq-take (nreverse align-diff-sweep--flickeriest) 12))
     (let ((shown (seq-take problems align-diff-sweep-report)))
       (mapcar (lambda (problem)
                 (format "%s at %s, place %S: %s"
                         (nth 0 problem) (nth 1 problem)
                         (nth 2 problem) (nth 3 problem)))
               shown))
     (when (> (length problems) align-diff-sweep-report)
       (list (format "... and %d more"
                     (- (length problems) align-diff-sweep-report)))))))

(defun align-diff-sweep--report-file ()
  "Return where to write the report."
  (or (getenv "ALIGN_DIFF_SWEEP_OUT")
      (expand-file-name "align-diff-sweep.txt" temporary-file-directory)))

(defun align-diff-sweep-batch ()
  "Sweep, write the report, and exit with a status."
  (let ((file (align-diff-sweep--report-file)))
    (run-with-timer
     1800 nil
     (lambda ()
       (with-temp-file file
         (dolist (line (align-diff-sweep--lines
                        (nreverse align-diff-sweep--problems)))
           (insert line "\n"))
         (insert "TIMED OUT\n"))
       (kill-emacs 2)))
    (run-with-idle-timer
     0.5 nil
     (lambda ()
       (align-buffer-background-frame)
       ;; Whatever happens, the report is written and the frame goes away: a
       ;; run left standing with an error in it says nothing from out here.
       (let ((problems (condition-case error (align-diff-sweep-run)
                         (error
                          (align-diff-sweep--note 'run-raised
                                                  align-diff-sweep--seen nil
                                                  (format "%S" error))
                          (nreverse align-diff-sweep--problems)))))
         (with-temp-file file
           (dolist (line (align-diff-sweep--lines problems))
             (insert line "\n")))
         (kill-emacs (if problems 1 0)))))))

(provide 'align-diff-sweep)

;;; align-diff-sweep.el ends here
