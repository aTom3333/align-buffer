;;; align-buffer-sync.el --- Keep the two buffers scrolled to each other -*- lexical-binding: t; -*-

;;; Commentary:

;; The window the user is in leads; the other follows.  The correspondence
;; decides where: we take the pair governing the line point is on and put that
;; pair's two lines on the same screen row.
;;
;; Anchoring on POINT rather than on the window's top line is what makes an
;; unpaired run behave.  While point moves down a block with no counterpart the
;; governing pair does not change, so the other window holds still instead of
;; scrolling through content that has nothing to answer to.  And once that
;; pair's own line scrolls off the top its row is taken as zero, so the other
;; window stops rather than drifting on by the height of the block.
;;
;; A correspondence may be non-monotonic, so the line the follower has to show
;; can be anywhere in its buffer.  Crossing into a moved block makes it jump,
;; which is the whole point of allowing it.
;;
;; Everything is counted in SCREEN rows, not buffer lines, so a soft-wrapped
;; line takes the room it actually takes and the anchor still lands level.
;;
;; FIVE RULES.  Every bug found in the synthetic-pane version of this file was
;; a breach of one of them, and they hold unchanged here:
;;
;; 1. Never act on a viewport reading older than the current redisplay.  Read
;;    the leader when the placement happens, not when it was decided.
;;
;; 2. Redisplay moves a window whose point is inside its `scroll-margin'.  Both
;;    windows are subject to this, so we place the follower's point clear of its
;;    margin AND read the leader's rows as they will settle rather than as they
;;    momentarily are.  Clear of the margin has to hold even where the follower
;;    runs out of lines to offer, which costs it the row we wanted: a placement
;;    redisplay undoes reads as the user scrolling, and the follower then leads
;;    and drags the leader back.
;;
;; 3. Setting a window start is what asks for that window to be painted.  Skip
;;    the write as redundant and the paint is skipped with it, so the paint is
;;    asked for explicitly instead.
;;
;; 4. A window we placed is not a scroll to follow.  Moving the follower scrolls
;;    it, which fires both hooks for it, and leading from that would undo what
;;    the user just did.  So we record the start and the point we imposed, and a
;;    window sitting on exactly those is left alone.  Reading that record does
;;    not clear it, so both hooks can ask in the same cycle and get the same
;;    answer.
;;
;; 5. Point cannot be moved from `window-scroll-functions'; redisplay reverts
;;    it.  A placement of the selected window is therefore queued, and carried
;;    out by a one-shot timer of no delay, so that it does not wait on the user
;;    pressing another key.

;;; Code:

(require 'align-buffer-core)

(defvar align-buffer--syncing nil
  "Non-nil while we are positioning the other window.")

(defvar align-buffer--in-redisplay nil
  "Non-nil while we run inside `window-scroll-functions'.")

(defvar align-buffer--pending-timer nil
  "The one-shot timer set to carry out a queued placement.")

(defvar align-buffer--pending-place nil
  "A queued placement, as (SESSION WINDOW SIDE LEADER).")

(defvar-local align-buffer--last-start nil
  "Window start this buffer's window had when we last looked.")

(defvar-local align-buffer--last-line-beginning nil
  "Line beginning point was at in this buffer when we last looked.")


;;; Measuring a window

(defun align-buffer--position-rows-below (window from rows)
  "Return the position ROWS screen rows below FROM in WINDOW.
Also how many rows that turned out to be, which is fewer against either end of
the buffer, as (POSITION . ROWS)."
  (with-current-buffer (window-buffer window)
    (save-excursion
      (goto-char from)
      (let ((moved (vertical-motion rows window)))
        (cons (point) moved)))))

(defun align-buffer--rows-from-start (window position)
  "Return the screen row POSITION sits on in WINDOW, or nil when it is above.
Nil as well when it is further below than the window is tall, so the count
always walks at most a windowful."
  ;; Counted by the same motion that does the placing.  `count-screen-lines'
  ;; reads a line wider than the window as two rows where `vertical-motion',
  ;; and the display, give a truncated line one, so measuring with one and
  ;; moving with the other puts the two windows a row apart for every long line
  ;; between the start and the anchor.
  (let ((start (window-start window))
        (height (max 1 (window-body-height window))))
    (when (>= position start)
      (with-current-buffer (window-buffer window)
        (save-excursion
          (goto-char start)
          (let ((row 0)
                (answer (and (= position start) 0)))
            (while (and (null answer) (< row height))
              (if (<= (vertical-motion 1 window) 0)
                  ;; Nothing below: whatever is left sits on this row.
                  (setq answer row)
                (cond ((> (point) position) (setq answer row))
                      ((= (point) position) (setq answer (1+ row)))
                      (t (setq row (1+ row))))))
            (when (and answer (< answer height)) answer)))))))

(defun align-buffer--rows-below-window (window wanted)
  "Return how many of WANTED screen rows sit below WINDOW's last row."
  (let* ((height (max 1 (window-body-height window)))
         (reached (cdr (align-buffer--position-rows-below
                        window (window-start window) (+ (1- height) wanted)))))
    (max 0 (- reached (1- height)))))

(defun align-buffer--rows-above-window (window wanted)
  "Return how many of WANTED screen rows sit above WINDOW's start."
  (- (cdr (align-buffer--position-rows-below
           window (window-start window) (- wanted)))))

(defun align-buffer--settling-correction (window)
  "Return the rows redisplay will scroll WINDOW by to clear its margin.
Positive scrolls forward."
  ;; Rule 2, read on the leader.  The wheel leaves point one row past the last
  ;; that clears the bottom margin, and redisplay then scrolls the window to fix
  ;; that; the correction lands while we are inside our own sync and deaf to it,
  ;; so a row measured against the start as it momentarily is leaves the pair a
  ;; row apart.
  ;;
  ;; A scroll needs rows to scroll into, and either end of the buffer can offer
  ;; fewer than the margin asks for: at the last line redisplay leaves the
  ;; window alone, and a correction predicted there is one the window never
  ;; makes, which moves the other window instead.
  (let* ((height (max 1 (window-body-height window)))
         (margin (align-buffer-effective-margin window))
         (point-row (align-buffer--rows-from-start window (window-point window)))
         (highest (max 0 (- height margin 1))))
    (cond ((null point-row) 0)
          ((> point-row highest)
           (let ((wanted (- point-row highest)))
             (min wanted (align-buffer--rows-below-window window wanted))))
          ((< point-row margin)
           (let ((wanted (- margin point-row)))
             (- (min wanted (align-buffer--rows-above-window window wanted)))))
          (t 0))))

(defun align-buffer--leader-row (window position)
  "Return the screen row POSITION will sit on in WINDOW once redisplay settles.
Zero when it is above the window's start: a pair scrolled off the top holds the
other window where it is rather than dragging it further."
  (let ((row (align-buffer--rows-from-start window position)))
    (if (null row)
        0
      (max 0 (- row (align-buffer--settling-correction window))))))


;;; What the follower has to show

(defun align-buffer--target (session side leader follower)
  "Return what FOLLOWER must show to sit level with LEADER, which shows SIDE.
As (POSITION . ROWS): the counterpart line's position and the screen row it
belongs on.  Nil when the correspondence says nothing about the line point is
on in LEADER."
  (let* ((leader-buffer (window-buffer leader))
         (line (align-buffer-line-at leader-buffer (window-point leader)))
         (anchor (and line (align-buffer-anchor session side line))))
    (when anchor
      (let* ((height (max 1 (window-body-height follower)))
             (other-position (align-buffer-line-position
                              (window-buffer follower) (cdr anchor)))
             (rows (if (> (car anchor) line)
                       ;; Point sits above every pair, so we show the first
                       ;; counterpart at the top and wait for point to reach it.
                       0
                     (align-buffer--leader-row
                      leader
                      (align-buffer-line-position leader-buffer (car anchor))))))
        (when other-position
          (cons other-position (max 0 (min (1- height) rows))))))))


;;; Whose scroll is it

(defun align-buffer--note-placed (window start point)
  "Remember the START and POINT we imposed on WINDOW."
  ;; Rule 4.  Both values, because we set both and either one changing is the
  ;; user moving.  A start-only record reads as ours after a point-motion
  ;; scroll, whose window start is still stale when the command hook runs, and
  ;; a record read as ours swallows the keypress.
  (set-window-parameter window 'align-buffer-placed (cons start point)))

(defun align-buffer--own-scroll-p (window)
  "Return non-nil when WINDOW sits where we put it rather than where a user did."
  ;; Answered by value, so both hooks can ask in the same cycle and get the
  ;; same answer.  A flag saying "we moved something recently" is cleared by
  ;; whichever command comes next, and when that is the user's own wheel notch
  ;; it costs them the notch.
  (let ((noted (window-parameter window 'align-buffer-placed)))
    (cond ((null noted) nil)
          ((and (= (car noted) (window-start window))
                (or (null (cdr noted))
                    (= (cdr noted) (window-point window))))
           t)
          (t (set-window-parameter window 'align-buffer-placed nil)
             nil))))

(defun align-buffer-forget-placement (window)
  "Forget that we ever placed WINDOW."
  (when (window-live-p window)
    (set-window-parameter window 'align-buffer-placed nil)))


;;; Placing a window

(defun align-buffer--set-point (window position)
  "Put WINDOW's point at POSITION so that it sticks."
  (if (eq window (selected-window))
      (with-current-buffer (window-buffer window) (goto-char position))
    (set-window-point window position)))

(defun align-buffer--clamp-point-row (row start-at-beginning height margin)
  "Return ROW moved as little as possible into the band redisplay leaves alone.
MARGIN rows stay clear at each edge of a HEIGHT-row window."
  ;; No floor at all when the window starts at the first line: the margin is
  ;; there to stop redisplay scrolling the window back, and there is nothing
  ;; above the first line to scroll into.  A floor there drags the follower's
  ;; point MARGIN rows down from a viewport that starts at the top.
  (let* ((lowest (if start-at-beginning 0 margin))
         (highest (max 0 (- height margin 1)))
         (wanted (max lowest (min highest row))))
    ;; A window shorter than twice its margin leaves no band at all, and a row
    ;; outside the window would be worse than one inside a margin.
    (max 0 (min (1- height) wanted))))

(defun align-buffer--start-leaving-room (window start rows)
  "Return START, moved back far enough that ROWS screen rows fit below it.
The end of the buffer leaves fewer, and then the row we wanted for the anchor
is not on offer: a point that near the bottom sits inside the margin, so
redisplay scrolls the window and takes our placement with it."
  (pcase-let ((`(_ . ,available)
               (align-buffer--position-rows-below window start rows)))
    (if (>= available rows)
        start
      (car (align-buffer--position-rows-below
            window start (- available rows))))))

(defun align-buffer--place (session window side leader)
  "Place WINDOW, which shows SIDE of SESSION, level with LEADER."
  ;; Rule 5.  A placement of the SELECTED window cannot be done from inside
  ;; redisplay: its point would be reverted, and a start whose point is still
  ;; inside the margin is one redisplay scrolls away by itself, so the two have
  ;; to land together or not at all.
  ;; Nil from the deferred branch, since nothing has moved yet: the timer that
  ;; carries the placement out runs with the command loop free, where an
  ;; ordinary redisplay paints what we marked.
  (if (and align-buffer--in-redisplay (eq window (selected-window)))
      (progn (align-buffer--defer-place session window side leader) nil)
    (align-buffer--place-now session window side leader)))

(defun align-buffer--place-now (session window side leader)
  "Place WINDOW, which shows SIDE of SESSION, level with LEADER.
Return non-nil when it moved, and nil when it was already there."
  (let ((target (align-buffer--target
                 session (align-buffer-other-side side) leader window)))
    (when target
      (pcase-let*
          ((`(,counterpart . ,rows) target)
           (`(,wanted . ,achieved)
            (align-buffer--position-rows-below window counterpart (- rows)))
           (buffer (window-buffer window))
           (height (max 1 (window-body-height window)))
           (margin (align-buffer-effective-margin window))
           (point-row (align-buffer--clamp-point-row
                       (- achieved)
                       (with-current-buffer buffer (= wanted (point-min)))
                       height margin))
           (start (align-buffer--start-leaving-room window wanted point-row))
           (point (car (align-buffer--position-rows-below
                        window start point-row))))
        ;; A buffer the reader has narrowed cannot be shown from outside the
        ;; part they left accessible, so we leave such a window alone rather
        ;; than writing a start redisplay would have to undo.
        (when (with-current-buffer buffer
                (and (>= start (point-min)) (<= start (point-max))))
          (let ((moved (not (align-buffer--already-placed-p window start point))))
            (when moved
              ;; Point first, so redisplay never sees the new start with a point
              ;; the margin would make it scroll away from.
              (align-buffer--set-point window point)
              (set-window-start window start))
            ;; Rule 4, and noted even when nothing was written: the window sits
            ;; where we want it either way, so its next scroll hook is our
            ;; doing.  Skipping it freezes the window.
            (align-buffer--note-placed window start (window-point window))
            ;; Rule 3, and asked for even when nothing was written: setting a
            ;; start is what marks a window for painting, so skipping the write
            ;; as redundant skips the paint too, and a follower already in the
            ;; right place goes on showing stale pixels.
            (force-window-update window)
            moved))))))

(defun align-buffer--already-placed-p (window start point)
  "Return non-nil when WINDOW already begins at START with its point at POINT."
  (and (= (window-start window) start)
       (= (window-point window) point)))


;;; Queueing a placement

(defun align-buffer--defer-place (session window side leader)
  "Queue a placement of WINDOW, and arrange for it to happen."
  (setq align-buffer--pending-place (list session window side leader))
  ;; A timer of no delay, which runs as soon as the command loop is free.
  ;; Waiting for the next command hook is not enough: a wheel notch over the
  ;; window the user is not in scrolls it, runs its command hook, and only THEN
  ;; reaches redisplay and us, so that hook has already been and gone.  The
  ;; placement would wait for the user's next keystroke, leaving the two a notch
  ;; apart for as long as they did nothing.
  (unless align-buffer--pending-timer
    (setq align-buffer--pending-timer
          (run-with-timer 0 nil #'align-buffer--flush-pending-place))))

(defun align-buffer-forget-pending-place (session)
  "Drop a queued placement of SESSION's, leaving another session's alone."
  (when (eq (nth 0 align-buffer--pending-place) session)
    (setq align-buffer--pending-place nil)
    (when align-buffer--pending-timer
      (cancel-timer align-buffer--pending-timer)
      (setq align-buffer--pending-timer nil))))

(defun align-buffer--flush-pending-place ()
  "Carry out a queued placement, from the timer set for it."
  (setq align-buffer--pending-timer nil)
  (align-buffer--apply-pending-place))

(defun align-buffer--apply-pending-place ()
  "Carry out a queued placement, if it is still the right thing to do."
  (when align-buffer--pending-place
    (let* ((pending align-buffer--pending-place)
           (session (nth 0 pending))
           (window (nth 1 pending))
           (side (nth 2 pending))
           (leader (nth 3 pending)))
      (setq align-buffer--pending-place nil)
      (when align-buffer--pending-timer
        (cancel-timer align-buffer--pending-timer)
        (setq align-buffer--pending-timer nil))
      (when (and (window-live-p window)
                 (window-live-p leader)
                 (align-buffer-session-live-p session)
                 (eq (window-buffer window) (align-buffer-buffer session side)))
        ;; Rule 1: the target is worked out again here, never carried along.
        ;; By now the leader has usually moved further, and a queued row would
        ;; leave the follower a notch behind and keep it there.
        (let ((align-buffer--syncing t))
          (align-buffer--place-now session window side leader))))))


;;; Syncing

(defconst align-buffer--least-window-height 4
  "Below this many rows a window is not treated as one of a session's.")

(defun align-buffer--window-big-enough-p (window)
  "Return non-nil when WINDOW is tall enough to take part."
  (and (window-live-p window)
       (>= (window-body-height window) align-buffer--least-window-height)))

(defun align-buffer--can-lead-p (window)
  "Return non-nil when WINDOW's viewport is worth following."
  ;; A buffer shown TWICE on one frame is one in the middle of being handed
  ;; around, and neither window is worth following: `display-buffer' makes its
  ;; window before it gives it a buffer, so for one redisplay the new window
  ;; shows what it was split from, with that window's start and a height of its
  ;; own.  The count is the signal because a window can only inherit a buffer
  ;; from a window that goes on showing it.
  (and (align-buffer--window-big-enough-p window)
       (= 1 (length (get-buffer-window-list (window-buffer window) nil
                                            (window-frame window))))))

(defun align-buffer--follower (session side)
  "Return the window showing the buffer facing SIDE, or nil."
  (let ((buffer (align-buffer-buffer session (align-buffer-other-side side))))
    (when (buffer-live-p buffer)
      (let ((windows (get-buffer-window-list buffer nil t)))
        (and (= (length windows) 1) (car windows))))))

(defun align-buffer-sync (window)
  "Align the window facing WINDOW's to WINDOW.
Return non-nil when the other window was moved."
  (unless align-buffer--syncing
    (let ((session (align-buffer-session-for (window-buffer window)))
          (side (buffer-local-value 'align-buffer--side (window-buffer window))))
      (when (and session side (align-buffer--can-lead-p window))
        (let ((follower (align-buffer--follower session side)))
          (when (align-buffer--window-big-enough-p follower)
            (let ((align-buffer--syncing t))
              (align-buffer--place session follower
                                   (align-buffer-other-side side)
                                   window))))))))


;;; Triggers

(defun align-buffer--scroll-hook (window _start)
  "Sync from WINDOW as redisplay scrolls it."
  (when (window-live-p window)
    ;; Errors demoted, which turns a signal from the body into a message and
    ;; carries on.  Emacs REMOVES a hook function that signals, so one error
    ;; would leave the two unsynced for the rest of the session rather than for
    ;; one redisplay.
    (with-demoted-errors "align-buffer scroll sync: %S"
      (let ((ours (align-buffer--own-scroll-p window))
            (align-buffer--in-redisplay t))
        (unless ours (align-buffer-sync window))))))

(defun align-buffer--post-command ()
  "Sync after a command that scrolled or changed line, and force both paints."
  (with-demoted-errors "align-buffer post-command sync: %S"
    (unless align-buffer--syncing
      (align-buffer--apply-pending-place)
      (let* ((window (selected-window))
             (start (window-start window))
             (line (line-beginning-position))
             (ours (align-buffer--own-scroll-p window)))
        ;; Same-line typing must cost nothing, hence the change test.  The line
        ;; beginning is watched as well as the start because a point-motion
        ;; scroll leaves the start stale here, and that is the only signal that
        ;; one is about to happen.
        (when (and (not ours)
                   (or (null align-buffer--last-start)
                       (/= start align-buffer--last-start)
                       (null align-buffer--last-line-beginning)
                       (/= line align-buffer--last-line-beginning)))
          (let ((session (align-buffer-current-session)))
            (when (and session
                       (window-live-p (align-buffer--follower
                                       session align-buffer--side)))
              (redisplay t)))
          ;; The rest is worth doing only when the follower actually moved AND
          ;; the command loop will not paint by itself, which it does not while
          ;; input waits: a held key, a burst of notches, a keyboard macro.  For
          ;; a single keystroke it paints every window as soon as we return, and
          ;; doing it ourselves costs two redisplays and a visible change of
          ;; face on both mode lines.  A follower that did not move may still
          ;; carry a mark from rule 3; that waits for the end of the burst.
          (when (and (align-buffer-sync window)
                     (or (input-pending-p) executing-kbd-macro))
            (let ((session (align-buffer-current-session)))
              (when session
                (let ((follower (align-buffer--follower session
                                                        align-buffer--side)))
                  (when (window-live-p follower)
                    ;; The follower has to be SELECTED for its pixels to reach
                    ;; the screen: with input waiting Emacs paints only the
                    ;; selected window, and neither `redisplay' nor
                    ;; `force-window-update' overrides that.  Bound, or the
                    ;; follower would sync back onto the leader and drag its
                    ;; point along.
                    (let ((align-buffer--syncing t))
                      (with-selected-window follower (redisplay t)))
                    ;; Selecting the follower also paints its mode line in the
                    ;; ACTIVE face, and by the time redisplay next asks what
                    ;; changed the selection is back where it started, so it
                    ;; finds nothing to repair and both read as active.
                    (dolist (one (list window follower))
                      (with-current-buffer (window-buffer one)
                        (force-mode-line-update)))
                    (redisplay t)))))))
        (setq align-buffer--last-start (window-start window))
        (setq align-buffer--last-line-beginning (line-beginning-position))))))

(defun align-buffer-sync-enable ()
  "Start syncing the current buffer."
  (add-hook 'window-scroll-functions #'align-buffer--scroll-hook nil t)
  (add-hook 'post-command-hook #'align-buffer--post-command nil t))

(defun align-buffer-sync-disable ()
  "Stop syncing the current buffer."
  (remove-hook 'window-scroll-functions #'align-buffer--scroll-hook t)
  (remove-hook 'post-command-hook #'align-buffer--post-command t)
  (kill-local-variable 'align-buffer--last-start)
  (kill-local-variable 'align-buffer--last-line-beginning))

(provide 'align-buffer-sync)

;;; align-buffer-sync.el ends here
