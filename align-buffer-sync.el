;;; align-buffer-sync.el --- Keep align-buffer's panes scrolled together -*- lexical-binding: t; -*-

;;; Commentary:

;; The pane the user is in leads; the other follows.  We align the CENTRE of the
;; two windows, which is the same as aligning their tops whenever the two heights
;; match.
;;
;; A pane truncates, so every row is one screen line and a viewport is described
;; by its top row alone.
;;
;; Two triggers.
;;
;; `window-scroll-functions' fires during redisplay, with the window start
;; already updated, so it sees a scroll whatever caused it.
;;
;; `post-command-hook' covers what that misses.  Emacs paints nothing while input
;; is pending, so a held key scrolls the leader many times over before the
;; follower is painted once; we ask for that paint ourselves.  And a command that
;; takes point out of the window is going to scroll it, but has not yet when the
;; hook runs: the window start still reads as the old one, so the line point sits
;; on is the only sign of what is coming.
;;
;; FIVE RULES.  Every bug found in this file so far has been a breach of one of
;; them, so they are worth stating before any of the code:
;;
;; 1. Never act on a viewport reading older than the current redisplay.  Read the
;;    leader when the placement happens, not when it was decided.
;;
;; 2. Redisplay moves a window whose point is inside its `scroll-margin'.  Both
;;    panes are subject to this, so we place the follower's point clear of its
;;    margin AND read the leader's viewport as it will settle rather than as it
;;    momentarily is.
;;
;; 3. Setting a window start is what asks for that window to be painted.  Skip
;;    the write as redundant and the paint is skipped with it, so the paint is
;;    asked for explicitly instead.
;;
;; 4. A window we placed is not a scroll to follow.  Moving the follower scrolls
;;    it, which fires both hooks for it, and leading from that would undo what
;;    the user just did.  So we record the start and the point we imposed, and a
;;    window sitting on exactly those is left alone.  Reading that record does not
;;    clear it, so both hooks can ask in the same cycle and get the same answer.
;;
;; 5. Point cannot be moved from `window-scroll-functions'; redisplay reverts it.
;;    A placement of the selected window is therefore queued, and carried out by a
;;    one-shot timer of no delay, so that it does not wait on the user pressing
;;    another key.

;;; Code:

(require 'align-buffer-core)

(defcustom align-buffer-sync-point nil
  "When non-nil, the follower's point sits on the leader's row.
When nil it is moved only when the follower's own margin would otherwise make
redisplay scroll the window."
  :type 'boolean
  :group 'align-buffer)

(defvar align-buffer--syncing nil
  "Non-nil while we are positioning the other pane.")

(defvar align-buffer--in-redisplay nil
  "Non-nil while we run inside `window-scroll-functions'.")

(defvar align-buffer--pending-timer nil
  "The one-shot timer set to carry out a queued placement.")

(defvar align-buffer--pending-place nil
  "A queued placement, as (SESSION WINDOW SIDE ROW LEADER).")

(defvar-local align-buffer--last-start nil
  "Window start this pane had when we last looked.")

(defvar-local align-buffer--last-line-beginning nil
  "Line beginning point was at in this pane when we last looked.")


;;; Reading a viewport

(defun align-buffer--row-at-point-in (window position)
  "Return the row at POSITION in WINDOW's buffer."
  (with-current-buffer (window-buffer window)
    (align-buffer-row-at-point position)))

(defun align-buffer--viewport-row (window)
  "Return the row at the top of WINDOW."
  (align-buffer--row-at-point-in window (window-start window)))

(defun align-buffer--settled-top (window)
  "Return the top row WINDOW will show once redisplay has had its say."
  ;; Rule 2, read on the leader.  The wheel leaves point one row past the last
  ;; that clears the bottom margin, and redisplay then scrolls the window to fix
  ;; that; the correction lands while we are inside our own sync and deaf to it,
  ;; so following the current top leaves the pair a row apart.
  (align-buffer--settled-top-row
   (align-buffer--viewport-row window)
   (window-body-height window)
   (align-buffer--effective-margin window)
   (align-buffer--row-at-point-in window (window-point window))))

(defun align-buffer--settled-top-row (top height margin point-row)
  "Return where a HEIGHT-row window at TOP settles with its point on POINT-ROW.
Scrolled by as little as it takes to leave MARGIN rows clear of POINT-ROW."
  (let ((lowest (+ top margin))
        (highest (- (+ top height) margin 1)))
    (max 0 (cond ((< point-row lowest) (- top (- lowest point-row)))
                 ((> point-row highest) (+ top (- point-row highest)))
                 (t top)))))

(defun align-buffer--target-row (leader follower)
  "Return the row FOLLOWER shows at its top to sit level with LEADER."
  (align-buffer--top-row-for-centre
   follower
   (align-buffer--centre-row leader (align-buffer--settled-top leader))))

(defun align-buffer--centre-row (window row)
  "Return the row at the middle of WINDOW when ROW is at its top."
  (+ row (/ (window-body-height window) 2)))

(defun align-buffer--top-row-for-centre (window centre)
  "Return the row that puts CENTRE at the middle of WINDOW."
  (- centre (/ (window-body-height window) 2)))

(defun align-buffer--clamp-point-row (row top height margin limit)
  "Return ROW moved as little as possible into a HEIGHT-row window at TOP.
MARGIN rows stay clear at each edge and the result stays within LIMIT."
  ;; Rule 2, read on the follower.  No floor at all when TOP is row zero: the
  ;; margin is there to stop redisplay scrolling the window back, and there is
  ;; nothing above row zero to scroll into.  A floor there drags the follower's
  ;; point MARGIN rows down from a viewport that starts at the top, which is
  ;; where `C-x o' lands the reader and where `visit-source' reads its line.
  (let (
        (lowest (if (> top 0) (+ top margin) 0))
        (highest (- (+ top height) margin 1)))
    (max 0 (min limit (max lowest (min highest row))))))

(defun align-buffer--point-row-for (session window top desired)
  "Return the row WINDOW's point should sit on once TOP is at its top.
DESIRED is the row we would like it on; nil keeps it near where it is."
  (align-buffer--clamp-point-row
   (or desired (align-buffer--row-at-point-in window (window-point window)))
   top
   (window-body-height window)
   (align-buffer--effective-margin window)
   (1- (align-buffer-row-count session))))

(defun align-buffer--leader-point-row (leader)
  "Return LEADER's point row, or nil when the point is not to be followed."
  (and align-buffer-sync-point
       (window-live-p leader)
       (align-buffer--row-at-point-in leader (window-point leader))))


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


;;; Placing a window

(defun align-buffer--set-point (window position)
  "Put WINDOW's point at POSITION so that it sticks."
  (if (eq window (selected-window))
      (with-current-buffer (window-buffer window) (goto-char position))
    (set-window-point window position)))

(defun align-buffer--already-placed-p (window start point)
  "Return non-nil when WINDOW already begins at START with its point at POINT."
  (and (= (window-start window) start)
       (or (null point)
           (= (align-buffer--row-at-point-in window point)
              (align-buffer--row-at-point-in window (window-point window))))))

(defun align-buffer--place (session window side row &optional leader)
  "Show SESSION's ROW at the top of WINDOW, which shows SIDE.
LEADER is the window being followed, whose point row this one adopts."
  ;; Rule 5.  A placement of the SELECTED window cannot be done from inside
  ;; redisplay: its point would be reverted, and a start whose point is still
  ;; inside the margin is one redisplay scrolls away by itself, so the two have
  ;; to land together or not at all.
  ;; Nil from the deferred branch, since nothing has moved yet: the timer
  ;; that carries the placement out runs with the command loop free, where an
  ;; ordinary redisplay paints what we marked.
  (if (and align-buffer--in-redisplay (eq window (selected-window)))
      (progn (align-buffer--defer-place session window side row leader) nil)
    (align-buffer--place-now session window side row leader)))

(defun align-buffer--defer-place (session window side row leader)
  "Queue a placement of WINDOW, and arrange for it to happen."
  (setq align-buffer--pending-place (list session window side row leader))
  ;; A timer of no delay, which runs as soon as the command loop is free.
  ;; Waiting for the next command hook is not enough: a wheel notch over the
  ;; pane the user is not in scrolls it, runs its command hook, and only THEN
  ;; reaches redisplay and us, so that hook has already been and gone.  The
  ;; placement would wait for the user's next keystroke, leaving the panes a
  ;; notch apart for as long as they did nothing.
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

(defun align-buffer-forget-placement (window)
  "Forget that we ever placed WINDOW."
  (when (window-live-p window)
    (set-window-parameter window 'align-buffer-placed nil)))

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
           (row (nth 3 pending))
           (leader (nth 4 pending)))
      (setq align-buffer--pending-place nil)
      (when align-buffer--pending-timer
        (cancel-timer align-buffer--pending-timer)
        (setq align-buffer--pending-timer nil))
      (when (and (window-live-p window)
                 (align-buffer-session-live-p session)
                 (eq (window-buffer window) (align-buffer-buffer session side)))
        (let ((align-buffer--syncing t))
          (align-buffer--place-now
           session window side
           ;; Rule 1: worked out again, never reused.  By now the leader has
           ;; usually scrolled further, and the queued row would leave the
           ;; follower a notch behind and keep it there.
           (if (window-live-p leader)
               (align-buffer--target-row leader window)
             row)
           leader))))))

(defun align-buffer--place-now (session window side row &optional leader)
  "Show SESSION's ROW at the top of WINDOW, point and start together.
Return non-nil when WINDOW was moved, and nil when it was already there."
  (let* ((top (align-buffer-clamp-row session row))
         (start (align-buffer-row-beginning-position session top side))
         (desired-point-row (align-buffer--leader-point-row leader))
         (point-row (align-buffer--point-row-for session window top
                                                 desired-point-row))
         (point (align-buffer-row-beginning-position session point-row side)))
    (when start
      (let ((moved (not (align-buffer--already-placed-p window start point))))
        (when moved
          ;; Point first, so redisplay never sees the new start with a point the
          ;; margin would make it scroll away from.  By row, never to the
          ;; position computed for the top: a point at a column far to the right
          ;; is what makes a pane scroll sideways of its own accord.
          (when point (align-buffer--set-point window point))
          (set-window-start window start))
        ;; Rule 4, and noted even when nothing was written: the window sits
        ;; where we want it either way, so its next scroll hook is our doing.
        ;; Skipping it freezes the pane.
        (align-buffer--note-placed window start (window-point window))
        ;; Rule 3, and asked for even when nothing was written: setting a start
        ;; is what marks a window for painting, so skipping the write as
        ;; redundant skips the paint too, and a follower already in the right
        ;; place goes on showing stale pixels for as long as the session lasts.
        (force-window-update window)
        moved))))


;;; Syncing

(defconst align-buffer--least-pane-height 4
  "Below this many rows a window is not treated as a pane.")

(defun align-buffer--pane-window-p (window)
  "Return non-nil when WINDOW is big enough to be a pane of a session."
  (and (window-live-p window)
       (>= (window-body-height window) align-buffer--least-pane-height)))

(defun align-buffer--can-lead-p (window)
  "Return non-nil when WINDOW's viewport is worth following."
  ;; A pane shown TWICE on one frame is a pane in the middle of being handed
  ;; around, and neither window is worth following: `display-buffer' makes its
  ;; window before it gives it a buffer, so for one redisplay the new window
  ;; shows what it was split from, with that window's start and a height of its
  ;; own.  Measured with the side window `which-key' puts up - a 17-row window
  ;; showing a pane whose real window was 30 - stubbing this out moves the
  ;; follower seven rows off the leader.  The count is the signal because a
  ;; window can only inherit a pane from a window that goes on showing it.
  (and (align-buffer--pane-window-p window)
       (= 1 (length (get-buffer-window-list (window-buffer window) nil
                                            (window-frame window))))))

(defun align-buffer--follower (session side)
  "Return the window showing the pane facing SIDE, or nil."
  (let ((buffer (align-buffer-buffer session (align-buffer-other-side side))))
    (when (buffer-live-p buffer)
      (let ((windows (get-buffer-window-list buffer nil t)))
        (and (= (length windows) 1) (car windows))))))

(defun align-buffer-sync (window)
  "Align the pane facing WINDOW's to WINDOW's viewport.
Return non-nil when the other pane was moved."
  (unless align-buffer--syncing
    (let ((session (buffer-local-value 'align-buffer--session (window-buffer window)))
          (side (buffer-local-value 'align-buffer--side (window-buffer window))))
      (when (and session side (align-buffer-session-live-p session)
                 (align-buffer--can-lead-p window))
        (let ((follower (align-buffer--follower session side)))
          (when (align-buffer--pane-window-p follower)
            (let* ((target (align-buffer--target-row window follower))
                   (align-buffer--syncing t))
              (let ((moved (align-buffer--place session follower
                                                (align-buffer-other-side side)
                                                target window)))
                (when (buffer-local-value 'truncate-lines (window-buffer window))
                  (set-window-hscroll follower (window-hscroll window)))
                moved))))))))


;;; Triggers

(defun align-buffer--scroll-hook (window _start)
  "Sync from WINDOW as redisplay scrolls it."
  (when (window-live-p window)
    ;; Errors demoted, which turns a signal from the body into a message and
    ;; carries on.  Emacs REMOVES a hook function that signals: measured, one
    ;; error and this is off `window-scroll-functions' for good, so a transient
    ;; failure would leave the panes unsynced for the rest of the session rather
    ;; than for one redisplay.
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
          (when (window-live-p
                 (let ((session (align-buffer-current-session)))
                   (and session
                        (align-buffer--follower session align-buffer--side))))
            (redisplay t))
          ;; The rest is worth doing only when the follower actually moved AND the
          ;; command loop will not paint by itself, which it does not while input
          ;; waits: a held key, a burst of notches, a keyboard macro.  For a
          ;; single keystroke it paints every window as soon as we return, and
          ;; doing it ourselves costs two redisplays and a visible change of face
          ;; on both mode lines.  A follower that did not move may still carry a
          ;; mark from rule 3; that waits for the end of the burst.
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
                    ;; finds nothing to repair and both panes read as active.
                    (dolist (pane (list window follower))
                      (with-current-buffer (window-buffer pane)
                        (force-mode-line-update)))
                    (redisplay t)))))))
        (setq align-buffer--last-start (window-start window))
        (setq align-buffer--last-line-beginning (line-beginning-position))))))

(defun align-buffer-sync-enable ()
  "Start syncing the current pane."
  (add-hook 'window-scroll-functions #'align-buffer--scroll-hook nil t)
  (add-hook 'post-command-hook #'align-buffer--post-command nil t))

(provide 'align-buffer-sync)

;;; align-buffer-sync.el ends here
