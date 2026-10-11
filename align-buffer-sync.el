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
;; Everything is measured in PIXELS, not buffer lines and not rows.  A line
;; takes the room the display gives it: wrapped over several rows, carrying rows
;; an overlay hangs above it, or taller than its neighbours because of its face.
;; None of that is visible to a count of lines, and a count of rows only works
;; while every row is worth the same.
;;
;; What we can ask of a window is coarser than what we can measure of it,
;; though, since a window start is a buffer position and so always a row
;; boundary.  The pixel asked for therefore often falls inside a row, and the
;; nearer boundary is taken.  Closing that gap needs `set-window-vscroll', and
;; redisplay drops a vscroll we set on the following cycle, so the last few
;; pixels are not on offer.
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
;;    asked for explicitly instead.  And asking from inside redisplay asks
;;    during the cycle already running, which honours nothing and schedules
;;    nothing, so a window moved there is marked again from a timer.
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

(defcustom align-buffer-exact-alignment nil
  "Whether a placement finishes with a vscroll, landing the pair on the pixel.
When nil the nearer row boundary is taken, which is up to half a row out and
settles in one go.

When non-nil the window is put on the boundary below the pixel wanted and
lifted onto it with a vscroll, which lands the pair exactly where a line is
drawn taller than a row, or wrapped differently on the two sides.  The lift
waits `align-buffer--lift-delay' for a redisplay to have been through, so the
window settles in two steps rather than one, and a block of overlay rows is
still out by rows the margin will not let the lift cover."
  :type 'boolean
  :group 'align-buffer)

(defvar align-buffer-post-place-functions nil
  "Functions run with the session, the window just placed, and its side.
Run when a placement moved the window.  No command runs in that window's
buffer, so whatever is drawn from its point is brought up to date from here.")

(defvar align-buffer--syncing nil
  "Non-nil while we are positioning the other window.")

(defvar align-buffer--in-redisplay nil
  "Non-nil while we run inside `window-scroll-functions'.")

(defvar align-buffer--pending-timer nil
  "The one-shot timer set to carry out a queued placement.")

(defvar align-buffer--pending-place nil
  "A queued placement, as (SESSION WINDOW SIDE LEADER).")

(defun align-buffer--lift-bound (window)
  "Return the largest lift worth giving WINDOW, in pixels.
A windowful: lifting further would hide more than the window shows, and the
rows a lift hides still have to be laid out.  Measured cost is flat up to
there, so nothing smaller buys anything."
  (* (max 1 (window-body-height window))
     (align-buffer--row-height window)))

(defconst align-buffer--lift-delay 0.05
  "How long a lift waits, so that a redisplay has been through before it.
A start written in one cycle clears the vscroll at the redisplay after it, and
a timer of no delay can run before that redisplay rather than after.")

(defvar align-buffer--pending-paint nil
  "Windows we have moved whose pixels are waiting for a redisplay.")

(defvar align-buffer--paint-timer nil
  "The one-shot timer set to ask for a queued paint.")


(defvar-local align-buffer--last-start nil
  "Window start this buffer's window had when we last looked.")

(defvar-local align-buffer--last-line-beginning nil
  "Line beginning point was at in this buffer when we last looked.")


;;; Measuring a window

(defun align-buffer--row-height (window)
  "Return what one screen row of WINDOW is worth in pixels."
  (with-current-buffer (window-buffer window)
    (max 1 (default-line-height))))

(defun align-buffer--pixels-between (window from to)
  "Return the height in pixels of WINDOW's text from FROM to TO."
  ;; `window-text-pixel-size' takes the window's vscroll off whatever it
  ;; reports, wherever the span begins, so it answers where text is drawn
  ;; rather than what it is worth.  Adding it back makes a span a span: a
  ;; measurement that moved with the lift already applied would have every lift
  ;; chasing the last one.
  (+ (cdr (window-text-pixel-size window (min from to) (max from to)))
     (window-vscroll window t)))

(defun align-buffer--rows-apart (window from to)
  "Return how many screen rows WINDOW puts between FROM and TO, FROM above TO.
Walked, and no further than the window is tall."
  (with-current-buffer (window-buffer window)
    (save-excursion
      (goto-char from)
      (let ((rows 0)
            (height (max 1 (window-body-height window))))
        (while (and (< (point) to)
                    (< rows height)
                    (not (zerop (vertical-motion 1 window))))
          (setq rows (1+ rows)))
        rows))))

(defun align-buffer--rows-from-start (window position)
  "Return the screen row POSITION sits on in WINDOW, or nil when it is above.
Nil as well when it is further below than the window is tall, so the measuring
covers at most a windowful."
  (let* ((start (window-start window))
         (height (max 1 (window-body-height window)))
         (row-height (align-buffer--row-height window)))
    (when (>= position start)
      ;; Everything above POSITION's own row, which is what the rows before it
      ;; in the window come to, including any an overlay hangs there.
      (let* ((limit (* (1+ height) row-height))
             (pixels (cdr (window-text-pixel-size window start position
                                                  nil limit)))
             (row (round pixels row-height)))
        (when (< row height) row)))))

(defun align-buffer--position-rows-below (window from rows)
  "Return the position ROWS screen rows below FROM in WINDOW, ROWS up when it
is negative.  Also how many rows that turned out to be, as (POSITION . ROWS),
which is fewer against either end of the buffer."
  ;; Walked rather than measured, because this is the margin's arithmetic and
  ;; not the alignment's: `vertical-motion' counts a wrapped line's rows and a
  ;; line drawn taller than its neighbours as the one row it is, and it is only
  ;; the rows an overlay adds that it walks past without counting.  Being a row
  ;; out about those costs a point placed a row nearer an edge than we meant,
  ;; where measuring instead would cost a pixel reading of the whole span on
  ;; every step of every placement.
  (with-current-buffer (window-buffer window)
    (save-excursion
      (goto-char from)
      (let ((moved (vertical-motion rows window)))
        (cons (point) moved)))))

(defun align-buffer--start-already-there (window position pixels)
  "Return (START . VSCROLL) using the start WINDOW already has, or nil.

Keeping the start and changing only the lift is what makes a placement land in
one go: a start written in one cycle clears the lift at the next, so the two
cannot be set together.  Nil when the start it has cannot be lifted onto the
pixel, or would have to be lifted further than a windowful, which is as far as
we are willing to lay out rows nobody is going to see."
  (let ((start (window-start window))
        (height (max 1 (window-body-height window))))
    (when (and align-buffer-exact-alignment (< start position))
      ;; Walked first, and no further than a windowful: a start further off
      ;; than that is both too far to lift from and too far to measure, since
      ;; the pixels have to be laid out to be counted.
      (when (< (align-buffer--rows-apart window start position) height)
        (let ((span (align-buffer--pixels-between window start position))
              (bound (align-buffer--lift-bound window)))
          (when (and (>= span pixels) (<= (- span pixels) bound))
            (cons start (- span pixels))))))))

(defun align-buffer--start-for-pixels (window position pixels)
  "Return a start for WINDOW putting POSITION PIXELS below its top, and what
is left over for a vscroll to take off, as (START . VSCROLL).

A start is a position and a position is a row boundary, so the pixel asked for
usually falls inside a row.  A vscroll only ever lifts the text, so the start
chosen is the boundary at or below the pixel and the remainder is positive;
with `align-buffer-exact-alignment' nil the nearer boundary is taken instead
and nothing is left over."
  (if (<= pixels 0)
      (cons position 0)
    (with-current-buffer (window-buffer window)
      (save-excursion
        ;; A guess at the usual row height, which is already the answer
        ;; wherever every row between the two is worth it, and a row or two out
        ;; where one is not.
        (goto-char (car (align-buffer--position-rows-below
                         window position
                         (- (round pixels (align-buffer--row-height window))))))
        (if (not align-buffer-exact-alignment)
            (cons (align-buffer--start-nearest window position pixels) 0)
          (let ((exact (align-buffer--start-at-or-below window position pixels))
                (bound (align-buffer--lift-bound window)))
            (if (<= (cdr exact) bound)
                exact
              ;; More lift than it is worth: the nearer boundary instead, which
              ;; lands in one go and in one step.
              (goto-char (car exact))
              (cons (align-buffer--start-nearest window position pixels) 0))))))))

(defun align-buffer--start-at-or-below (window position pixels)
  "Return the start nearest PIXELS without being above it, and the rest.
Point begins at a guess and the walk is from there, as (START . VSCROLL)."
  (let ((best (point))
        (best-pixels (align-buffer--pixels-between window (point) position))
        (done nil))
    (if (>= best-pixels pixels)
        ;; Far enough already: down while the next one is still far enough.
        (while (not done)
          (let ((here (point)))
            ;; Never past POSITION itself.  A span is measured between two
            ;; places whichever way round they are, so a candidate below it
            ;; measures the way back up and reads as far enough again, and the
            ;; walk would carry on to the end of the buffer.
            (if (or (zerop (vertical-motion 1 window))
                    (>= (point) position))
                (progn (goto-char here) (setq done t))
              (let ((got (align-buffer--pixels-between window (point) position)))
                (if (>= got pixels)
                    (setq best (point) best-pixels got)
                  (goto-char here)
                  (setq done t))))))
      ;; Too near: up until it is far enough, and stop there.
      (while (not done)
        (if (zerop (vertical-motion -1 window))
            (setq done t)
          (setq best (point))
          (setq best-pixels (align-buffer--pixels-between
                             window (point) position))
          (when (>= best-pixels pixels) (setq done t)))))
    (cons best (max 0 (- best-pixels pixels)))))

(defun align-buffer--start-nearest (window position pixels)
  "Return the start putting POSITION nearest PIXELS below the top of WINDOW.
Point begins at a guess and the walk is from there."
  (let ((best (point))
        (best-pixels (align-buffer--pixels-between window (point) position))
        (done nil))
    (unless (= best-pixels pixels)
      (let ((step (if (< best-pixels pixels) -1 1))
            (best-out (abs (- best-pixels pixels))))
        (while (not done)
          ;; Never past POSITION itself: a span is measured between two places
          ;; whichever way round they are, so a candidate below it reads as a
          ;; span again rather than as nothing.
          (if (or (zerop (vertical-motion step window))
                  (and (> step 0) (>= (point) position)))
              (setq done t)
            (let* ((got (align-buffer--pixels-between window (point) position))
                   (out (abs (- got pixels))))
              (when (< out best-out)
                (setq best (point))
                (setq best-out out))
              ;; Past the pixel asked for, and further on only gets worse.
              (when (if (< step 0) (>= got pixels) (<= got pixels))
                (setq done t)))))))
    best))

(defun align-buffer--leader-pixels (window position)
  "Return how far below WINDOW's top POSITION is drawn, in pixels.
In pixels, since a row is only worth its own height and two windows share
neither their rows nor the heights of them.  Zero when POSITION is above the
start, which is what holds the other window still rather than dragging it on.

Where the window is drawn now, with nothing added for where the margin might
take it next: a command moves point to the row its margin allows and leaves the
start alone, and redisplay scrolls a window it has already drawn for no margin
of either edge, so there is nothing further coming to allow for."
  (let ((start (window-start window))
        (height (max 1 (window-body-height window))))
    (if (< position start)
        0
      (max 0 (cdr (window-text-pixel-size
                   window start position nil
                   (* (1+ height) (align-buffer--row-height window))))))))


;;; What the follower has to show

(defun align-buffer--target (session side leader follower)
  "Return what FOLLOWER must show to sit level with LEADER, which shows SIDE.
As (POSITION . PIXELS): the counterpart line's position and how far below the
window's top it belongs.  Nil when the correspondence says nothing about the
line point is on in LEADER."
  (let* ((leader-buffer (window-buffer leader))
         (line (align-buffer-line-at leader-buffer (window-point leader)))
         (anchor (and line (align-buffer-anchor session side line))))
    (when anchor
      (let* ((height (max 1 (window-body-height follower)))
             (row-height (align-buffer--row-height follower))
             (other-position (align-buffer-line-position
                              (window-buffer follower) (cdr anchor)))
             (pixels (if (> (car anchor) line)
                         ;; Point sits above every pair, so we show the first
                         ;; counterpart at the top and wait for point to reach
                         ;; it.
                         0
                       (align-buffer--leader-pixels
                        leader
                        (align-buffer-line-position leader-buffer (car anchor))))))
        (when other-position
          (cons other-position
                (max 0 (min (* (1- height) row-height) pixels))))))))


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

(defun align-buffer--follower-point (window counterpart row point-row)
  "Return the point to give WINDOW, whose pair is drawn on ROW.
COUNTERPART itself when the margin leaves it on that row, since that is the
line being read; otherwise as many rows from COUNTERPART as the margin moved
it, which is as near to the pair as the margin allows.

Counted from the pair rather than from the window's start, which is the only
way of counting that is not thrown out by a lift: the rows a lift hides are
often rows an overlay added, and a walk cannot see those at all."
  (if (= row point-row)
      counterpart
    (car (align-buffer--position-rows-below window counterpart
                                            (- point-row row)))))

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

(defun align-buffer--defer-paint (window)
  "Ask for WINDOW to be painted once the command loop is free."
  (unless (memq window align-buffer--pending-paint)
    (push window align-buffer--pending-paint))
  (unless align-buffer--paint-timer
    (setq align-buffer--paint-timer
          (run-with-timer 0 nil #'align-buffer--flush-pending-paint))))

(defun align-buffer--flush-pending-paint ()
  "Mark the windows waiting for a paint, from the timer set for them."
  (setq align-buffer--paint-timer nil)
  (let ((windows align-buffer--pending-paint))
    (setq align-buffer--pending-paint nil)
    (dolist (window windows)
      (when (window-live-p window)
        (force-window-update window)))))

(defun align-buffer-forget-vscroll (window)
  "Put WINDOW back to no vscroll."
  (when (window-live-p window)
    (set-window-vscroll window 0 t)))

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
    (let ((moved (align-buffer--place-now session window side leader)))
      ;; Rule 3 again, from the other end.  A paint asked for from inside
      ;; redisplay is asked for during the cycle already running, so the mark is
      ;; not honoured and nothing schedules another one: the window sits where
      ;; we put it while the screen goes on showing where it was.  The timer
      ;; marks it with the command loop free, where it is painted.
      (when (and moved align-buffer--in-redisplay)
        (align-buffer--defer-paint window))
      moved)))

(defun align-buffer--place-now (session window side leader)
  "Place WINDOW, which shows SIDE of SESSION, level with LEADER.
Return non-nil when it moved, and nil when it was already there."
  (let ((target (align-buffer--target
                 session (align-buffer-other-side side) leader window)))
    (when target
      (pcase-let*
          ((`(,counterpart . ,pixels) target)
           (`(,wanted . ,vscroll)
            (or (align-buffer--start-already-there window counterpart pixels)
                (align-buffer--start-for-pixels window counterpart pixels)))
           (buffer (window-buffer window))
           (height (max 1 (window-body-height window)))
           (margin (align-buffer-effective-margin window))
           ;; The row the pair is DRAWN on, which is what the margin counts and
           ;; is what we are about to put it on.  Divided, as the margin's
           ;; arithmetic is throughout: it decides only whether point has to
           ;; come off the pair, and by how many rows.
           (row (round pixels (align-buffer--row-height window)))
           (point-row (align-buffer--clamp-point-row
                       row
                       (with-current-buffer buffer (= wanted (point-min)))
                       height margin))
           (start (align-buffer--start-leaving-room window wanted point-row))
           (point (align-buffer--follower-point window counterpart row point-row))
           ;; Moving the start back for the point row scrolls the window by
           ;; whole rows, and the remainder we measured is no longer its.
           (vscroll (if (= start wanted) vscroll 0)))
        ;; A buffer the reader has narrowed cannot be shown from outside the
        ;; part they left accessible, so we leave such a window alone rather
        ;; than writing a start redisplay would have to undo.
        (when (with-current-buffer buffer
                (and (>= start (point-min)) (<= start (point-max))))
          (let ((moved (not (align-buffer--already-placed-p
                             window start point vscroll)))
                (start-stays (= (window-start window) start)))
            (when moved
              ;; Point first, so redisplay never sees the new start with a point
              ;; the margin would make it scroll away from.
              (align-buffer--set-point window point)
              (cond
               (start-stays
                ;; Nothing of ours to clear the vscroll, so it can go on now.
                (set-window-vscroll window vscroll t)
                ;; A start written a cycle ago still clears the vscroll at the
                ;; redisplay after it, which can land after this.  Asking to be
                ;; called again settles it: a lift still in place is no change
                ;; and asks for nothing more, and a lift that was wiped is put
                ;; back.
                (when (> vscroll 0)
                  (align-buffer--defer-place session window side leader
                                             align-buffer--lift-delay)))
               (t
                (set-window-start window start)
                (set-window-vscroll window 0 t)
                ;; The start we just wrote takes the vscroll with it at the next
                ;; redisplay, so the lift has to wait for the far side of that.
                ;; Queued as a placement and not as a number of pixels: by then
                ;; the leader has usually moved again, and rule 1 says a figure
                ;; carried across a redisplay is a figure out of date.  The
                ;; second pass finds the start already right and lifts at once.
                (when (> vscroll 0)
                  (align-buffer--defer-place session window side leader
                                             align-buffer--lift-delay)))))
            ;; Rule 4, and noted even when nothing was written: the window sits
            ;; where we want it either way, so its next scroll hook is our
            ;; doing.  Skipping it freezes the window.
            (align-buffer--note-placed window start (window-point window))
            ;; Rule 3, and asked for even when nothing was written: setting a
            ;; start is what marks a window for painting, so skipping the write
            ;; as redundant skips the paint too, and a follower already in the
            ;; right place goes on showing stale pixels.
            (force-window-update window)
            (when moved
              ;; Last, so that the window is where we want it and marked before
              ;; anyone else looks.  Errors demoted, since we may be inside
              ;; `window-scroll-functions', where Emacs drops a hook that
              ;; signals and the two would stay unsynced from then on.
              (with-demoted-errors "align-buffer post-place: %S"
                (run-hook-with-args 'align-buffer-post-place-functions
                                    session window side)))
            moved))))))

(defun align-buffer--already-placed-p (window start point vscroll)
  "Return non-nil when WINDOW already begins at START, lifted VSCROLL, point
at POINT."
  (and (= (window-start window) start)
       (= (window-point window) point)
       (= (window-vscroll window t) vscroll)))


;;; Queueing a placement

(defun align-buffer--defer-place (session window side leader &optional delay)
  "Queue a placement of WINDOW, and arrange for it to happen.
DELAY waits that many seconds, which is how a placement waits for a redisplay
rather than merely for the command loop."
  (setq align-buffer--pending-place (list session window side leader))
  ;; A timer of no delay, which runs as soon as the command loop is free.
  ;; Waiting for the next command hook is not enough: a wheel notch over the
  ;; window the user is not in scrolls it, runs its command hook, and only THEN
  ;; reaches redisplay and us, so that hook has already been and gone.  The
  ;; placement would wait for the user's next keystroke, leaving the two a notch
  ;; apart for as long as they did nothing.
  (unless align-buffer--pending-timer
    (setq align-buffer--pending-timer
          (run-with-timer (or delay 0) nil
                          #'align-buffer--flush-pending-place))))

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
