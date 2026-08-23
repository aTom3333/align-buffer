;;; align-buffer-integration.el --- Scroll sync against a real display -*- lexical-binding: t; -*-

;;; Commentary:

;; Batch Emacs never redisplays, so `window-scroll-functions' never fires there
;; and the scroll sync cannot be tested at all.  These scenarios therefore run in
;; a graphical Emacs, driving REAL input through `execute-kbd-macro' so that the
;; command loop and its hooks run for themselves, and synthesising wheel events
;; as `(wheel-down POSITION 1)' vectors.
;;
;; Two things this cannot do.  It cannot see paint: every assertion here reads
;; window state, and a window whose start is right while its pixels are stale
;; looks perfect from Lisp.  And it cannot test what a config contributes, since
;; it runs with -Q; evil, hydra and the rest are absent.
;;
;; The scroll configuration below is not decoration.  With the -Q default of
;; `scroll-conservatively' at 0, Emacs recentres instead of scrolling by a line,
;; and the whole class of bug these scenarios exist for lives in the single-line
;; path.  A run on the defaults quietly tests a different Emacs.
;;
;; Run it with:
;;
;;   emacs -Q -L . -L test -l test/align-buffer-integration.el \
;;         -f align-buffer-integration-batch
;;
;; A frame appears for a second or two.  Each scenario prints a line, and the
;; process exits non-zero if any assertion failed.

;;; Code:

(require 'align-buffer)
(require 'cl-lib)

(defvar align-buffer-integration--failures nil
  "Descriptions of what went wrong, newest first.")

(defvar align-buffer-integration--scenarios nil
  "Scenarios to run, as (NAME . FUNCTION), in the order defined.")

(defmacro align-buffer-integration-defscenario (name &rest body)
  "Define scenario NAME, whose BODY drives input and calls `expect'."
  (declare (indent 1))
  `(add-to-list 'align-buffer-integration--scenarios
                (cons ',name (lambda () ,@body))
                t))


;;; Saying what happened

(defun align-buffer-integration--expect (scenario what wanted got)
  "Record whether WANTED and GOT agree for WHAT in SCENARIO."
  (unless (equal wanted got)
    (push (format "%s: %s wanted %S, got %S" scenario what wanted got)
          align-buffer-integration--failures)))

(defun align-buffer-integration--starts (session)
  "Return both panes' top rows as (LEFT RIGHT)."
  (mapcar (lambda (side)
            (let* ((buffer (align-buffer-buffer session side))
                   (window (get-buffer-window buffer)))
              (and window
                   (with-current-buffer buffer
                     (align-buffer-row-at-point (window-start window))))))
          '(left right)))

(defun align-buffer-integration--points (session)
  "Return both panes' point rows as (LEFT RIGHT)."
  (mapcar (lambda (side)
            (let* ((buffer (align-buffer-buffer session side))
                   (window (get-buffer-window buffer)))
              (and window
                   (with-current-buffer buffer
                     (align-buffer-row-at-point (window-point window))))))
          '(left right)))

(defun align-buffer-integration--aligned-p (session)
  "Return non-nil when both panes show the same top row."
  (let ((starts (align-buffer-integration--starts session)))
    (and (car starts) (equal (car starts) (cadr starts)))))


;;; Driving the display

(defun align-buffer-integration--settle ()
  "Let redisplay run and any no-delay timer fire."
  (redisplay t)
  (sit-for 0.05)
  (redisplay t))

(defun align-buffer-integration--wheel (window direction)
  "Send one WHEEL event of DIRECTION over WINDOW."
  (let ((position (with-selected-window window
                    (posn-at-point (window-point window) window))))
    (execute-kbd-macro (vector (list direction position 1)))
    (align-buffer-integration--settle)))

(defun align-buffer-integration--wheel-without-settling (window direction)
  "Send one WHEEL event over WINDOW and let nothing settle."
  (let ((position (with-selected-window window
                    (posn-at-point (window-point window) window))))
    (execute-kbd-macro (vector (list direction position 1)))))

(defun align-buffer-integration--leave-point-in-the-margin (session rows-down)
  "Scroll the selected pane ROWS-DOWN with its point inside the bottom margin.

Start and point are set together, which is what the wheel does and what a script
cannot do in two steps: redisplay would correct the point in between.  From here
redisplay will scroll the pane by itself, so a sync that reads the top as it
momentarily is follows a viewport that is about to move."
  (let* ((window (selected-window))
         (side (buffer-local-value 'align-buffer--side (window-buffer window)))
         (height (window-body-height window))
         (margin (align-buffer--effective-margin window))
         (top rows-down)
         (point-row (- (+ top height) margin)))
    (set-window-start window (align-buffer-row-beginning-position
                              session top side))
    (set-window-point window (align-buffer-row-beginning-position
                              session point-row side))))

(defun align-buffer-integration--keys (keys)
  "Type KEYS, a `kbd' string, through the command loop."
  (execute-kbd-macro (kbd keys))
  (align-buffer-integration--settle))

(defun align-buffer-integration--session ()
  "Build a session of 200 rows and return it, with the right pane selected."
  (let* ((text (mapconcat (lambda (number) (format "line %d of the file" number))
                          (number-sequence 1 200) "\n"))
         (left (generate-new-buffer " *integration left source*"))
         (right (generate-new-buffer " *integration right source*")))
    (dolist (buffer (list left right))
      (with-current-buffer buffer (insert text) (goto-char (point-min))))
    (delete-other-windows)
    (align-buffer-show
     (align-buffer-plan-create
      :left-name "*integration left*"
      :right-name "*integration right*"
      :rows (vconcat
             (cl-loop for line from 1 to 200
                      collect (align-buffer-row-create
                               :left (align-buffer-cell-create
                                      :kind 'line :source left :line line)
                               :right (align-buffer-cell-create
                                       :kind 'line :source right :line line))))))))

(defun align-buffer-integration--point-to-bottom-edge (session)
  "Put point on the last row of the selected pane that does not yet scroll."
  (let* ((window (selected-window))
         (side (buffer-local-value 'align-buffer--side (window-buffer window)))
         (top (align-buffer-row-at-point-in-window window))
         (height (window-body-height window))
         (margin (align-buffer--effective-margin window)))
    (with-selected-window window
      (goto-char (align-buffer-row-beginning-position
                  session (- (+ top height) margin 1) side)))
    (align-buffer-integration--settle)))

(defun align-buffer-row-at-point-in-window (window)
  "Return the top row of WINDOW."
  (with-current-buffer (window-buffer window)
    (align-buffer-row-at-point (window-start window))))

(defun align-buffer-integration--other-window (session)
  "Return the window showing the pane the user is not in."
  (let ((side (buffer-local-value 'align-buffer--side
                                 (window-buffer (selected-window)))))
    (get-buffer-window
     (align-buffer-buffer session (align-buffer-other-side side)))))


;;; The scenarios

;; Does NOT discriminate: checked with the forced follower redisplay and the
;; forced mode line update both stubbed out, and every scenario still passed.
;; What that fix produces is pixels, and this harness reads window state.  Kept
;; because a run of line motions is the commonest gesture there is.
(align-buffer-integration-defscenario held-key
  (let ((session (align-buffer-integration--session)))
    (align-buffer-integration--keys "C-n C-n C-n C-n C-n C-n C-n C-n")
    (align-buffer-integration--expect
     'held-key "panes aligned after eight line motions"
     t (align-buffer-integration--aligned-p session))
    (align-buffer-integration--keys "C-p C-p C-p")
    (align-buffer-integration--expect
     'held-key "panes aligned after moving back"
     t (align-buffer-integration--aligned-p session))
    (align-buffer-quit session)))

;; Does NOT discriminate, and is kept as ordinary regression coverage.  Checked:
;; it passes with `align-buffer--settled-top' stubbed back to the raw viewport
;; row.  The bug needs redisplay's correction of the leader to arrive while we
;; are inside our own sync, which happens only through the forced redisplay in
;; `post-command-hook'; a script redisplaying at top level is outside that guard,
;; so the correction is simply followed.  The evidence for that fix is the log
;; from a live session: the leader moved 17 to 18 with `:syncing t' set, and the
;; misalignment the user could reproduce went away.
(align-buffer-integration-defscenario wheel-over-the-active-pane
  (let ((session (align-buffer-integration--session)))
    (dotimes (_ 4)
      (align-buffer-integration--wheel (selected-window) 'wheel-down))
    (align-buffer-integration--expect
     'wheel-over-the-active-pane "panes aligned after a burst"
     t (align-buffer-integration--aligned-p session))
    (dotimes (_ 3)
      (align-buffer-integration--wheel (selected-window) 'wheel-up))
    (align-buffer-integration--expect
     'wheel-over-the-active-pane "panes aligned scrolling back"
     t (align-buffer-integration--aligned-p session))
    (align-buffer-quit session)))

;; Does NOT discriminate either, for the same reason, and was written to try.
;; Setting a start and a point that redisplay will correct does reproduce the
;; STATE, but the correction then arrives with nothing suppressing our response
;; to it.  Kept because it exercises a viewport that is about to move, which
;; nothing else here does.
(align-buffer-integration-defscenario a-leader-about-to-be-corrected
  (let ((session (align-buffer-integration--session)))
    (align-buffer-integration--leave-point-in-the-margin session 20)
    (align-buffer-integration--settle)
    (align-buffer-integration--expect
     'a-leader-about-to-be-corrected "panes aligned once redisplay has settled"
     t (align-buffer-integration--aligned-p session))
    (align-buffer-quit session)))

;; Written to catch the frozen row and does NOT: checked with
;; `align-buffer--apply-pending-place' stubbed back to using the queued row.
;; Sending notches without settling still lets each one's command hook run, and
;; that flushes the queue before the next arrives.  The evidence for that fix is
;; again a live log, which showed `place-now requested 3' while the leader had
;; already reached row 6.
(align-buffer-integration-defscenario notches-arriving-faster-than-we-place
  (let* ((session (align-buffer-integration--session))
         (other (align-buffer-integration--other-window session)))
    (dotimes (_ 5)
      (align-buffer-integration--wheel-without-settling other 'wheel-down))
    (align-buffer-integration--settle)
    (align-buffer-integration--expect
     'notches-arriving-faster-than-we-place "aligned once the burst is over"
     t (align-buffer-integration--aligned-p session))
    (align-buffer-quit session)))

;; Fails without: the one-shot timer in `align-buffer--defer-place'.  The command
;; hook of the notch has already run by the time redisplay reaches us, so nothing
;; would carry out the queued placement until the user's next keystroke.
(align-buffer-integration-defscenario wheel-over-the-inactive-pane
  (let* ((session (align-buffer-integration--session))
         (other (align-buffer-integration--other-window session)))
    (dotimes (index 4)
      (align-buffer-integration--wheel other 'wheel-down)
      (align-buffer-integration--expect
       'wheel-over-the-inactive-pane (format "aligned after notch %d" (1+ index))
       t (align-buffer-integration--aligned-p session)))
    (align-buffer-quit session)))

;; Fails without: the one-shot timer, along with the scenario above.  It does not
;; discriminate for the frozen-row fix: `execute-kbd-macro' returns before the
;; next notch is sent, so no notch arrives while a placement is still queued.
(align-buffer-integration-defscenario wheel-burst-over-the-inactive-pane
  (let* ((session (align-buffer-integration--session))
         (other (align-buffer-integration--other-window session)))
    (dotimes (_ 6) (align-buffer-integration--wheel other 'wheel-down))
    (align-buffer-integration--expect
     'wheel-burst-over-the-inactive-pane "aligned at the end of the burst"
     t (align-buffer-integration--aligned-p session))
    (align-buffer-quit session)))

;; Fails without: recording the point as well as the start in
;; `align-buffer--note-placed'.  Point sync stops after a placement, because a
;; start-only record reads as ours while the point has moved.
;;
;; Tracking bound on, since that is the behaviour under test rather than the
;; default.
(align-buffer-integration-defscenario point-follows-after-a-placement
  (let* ((session (align-buffer-integration--session))
         (other (align-buffer-integration--other-window session))
         (align-buffer-sync-point t))
    (align-buffer-integration--wheel other 'wheel-down)
    (align-buffer-integration--keys "C-n C-n C-n")
    (let ((points (align-buffer-integration--points session)))
      (align-buffer-integration--expect
       'point-follows-after-a-placement "both cursors on one row"
       (car points) (cadr points)))
    (align-buffer-quit session)))

;; Fails without: the missing floor at row zero in `align-buffer--clamp-point-row'.
;; With both panes at the top of the plan, a point move in one must not drag the
;; other's point `scroll-margin' rows down, since nothing above row zero can make
;; redisplay scroll the window back.
(align-buffer-integration-defscenario the-top-of-the-plan-holds-its-point
  (let* ((session (align-buffer-integration--session))
         (follower (align-buffer-integration--other-window session)))
    (align-buffer-integration--expect
     'the-top-of-the-plan-holds-its-point "both panes start at row zero"
     '(0 0) (align-buffer-integration--starts session))
    (align-buffer-integration--keys "C-n C-n C-n C-n C-n")
    (align-buffer-integration--expect
     'the-top-of-the-plan-holds-its-point "the follower's point stayed on row zero"
     0 (with-current-buffer (window-buffer follower)
         (align-buffer-row-at-point (window-point follower))))
    (align-buffer-quit session)))

;; The other side of that option, and the reason it is off: a point move that
;; scrolls nothing leaves the follower entirely alone, so nothing about it is
;; written and nothing of it is repainted.
(align-buffer-integration-defscenario a-safe-point-is-left-alone
  (let* ((session (align-buffer-integration--session))
         (align-buffer-sync-point nil)
         (follower (align-buffer-integration--other-window session))
         (where (lambda ()
                  (with-current-buffer (window-buffer follower)
                    (list (align-buffer-row-at-point (window-start follower))
                          (align-buffer-row-at-point (window-point follower))))))
         (before nil))
    (align-buffer-integration--keys "C-v C-v")
    (setq before (funcall where))
    (align-buffer-integration--keys "C-n C-n C-n")
    (align-buffer-integration--expect
     'a-safe-point-is-left-alone "the follower's start and point both untouched"
     before (funcall where))
    (align-buffer-integration--keys "C-v")
    (align-buffer-integration--expect
     'a-safe-point-is-left-alone "and a scroll still syncs"
     t (align-buffer-integration--aligned-p session))
    (align-buffer-quit session)))

;; Does NOT discriminate: checked under the `placed-flag' break, which puts the
;; consumed-flag shape back, and this passes.  Settling after each notch lets the
;; scroll hook sync from the leader, and that path never asks the question this
;; scenario is about.  `a-notch-after-a-placement' below is the one that catches
;; it.
(align-buffer-integration-defscenario one-notch-on-each-pane
  (let* ((session (align-buffer-integration--session))
         (other (align-buffer-integration--other-window session)))
    (dotimes (_ 3) (align-buffer-integration--wheel other 'wheel-down))
    (align-buffer-integration--wheel (selected-window) 'wheel-down)
    (align-buffer-integration--expect
     'one-notch-on-each-pane "the single notch on the active pane synced"
     t (align-buffer-integration--aligned-p session))
    (align-buffer-quit session)))

;; Fails without BOTH window guards, since either one catches this: a split
;; leaves a one-line window showing the pane, which is too short to be a pane and
;; is also a second window showing one.
(align-buffer-integration-defscenario a-transient-window-does-not-lead
  (let* ((session (align-buffer-integration--session))
         (before nil))
    (align-buffer-integration--keys "C-v")
    (setq before (align-buffer-integration--starts session))
    (let ((transient (split-window (selected-window) -2 'below)))
      (align-buffer-integration--settle)
      (delete-window transient))
    (align-buffer-integration--settle)
    (align-buffer-integration--expect
     'a-transient-window-does-not-lead "panes still aligned"
     t (align-buffer-integration--aligned-p session))
    (align-buffer-integration--expect
     'a-transient-window-does-not-lead "and did not jump"
     before (align-buffer-integration--starts session))
    (align-buffer-quit session)))

;;; Fails without: `align-buffer--can-lead-p'.  `which-key' puts its keys up in a
;; side window, and `display-buffer-in-side-window' makes that window before it
;; gives it a buffer, so for one redisplay it shows the pane its parent was
;; showing, at its own height.
(align-buffer-integration-defscenario a-side-window-does-not-lead
  (let* ((session (align-buffer-integration--session))
         (before nil)
         (keys (generate-new-buffer " *integration side*")))
    (align-buffer-integration--keys "C-v")
    (setq before (align-buffer-integration--starts session))
    (with-current-buffer keys
      (insert "SPC f  file\nSPC b  buffer\nSPC p  project\n"))
    (display-buffer-in-side-window
     keys '((side . bottom) (slot . 0)
            (window-height . (lambda (window) (fit-window-to-buffer window nil 1)))))
    (align-buffer-integration--settle)
    (align-buffer-integration--expect
     'a-side-window-does-not-lead "panes still aligned"
     t (align-buffer-integration--aligned-p session))
    (align-buffer-integration--expect
     'a-side-window-does-not-lead "and did not jump"
     before (align-buffer-integration--starts session))
    (delete-windows-on keys)
    (kill-buffer keys)
    (align-buffer-quit session)))

;; Fails without: looking windows up by buffer every time.  A pane may be off
;; screen, and the sync must then do nothing rather than signal.
(align-buffer-integration-defscenario a-pane-off-screen
  (let* ((session (align-buffer-integration--session))
         (other (align-buffer-integration--other-window session))
         (elsewhere (generate-new-buffer " *integration elsewhere*")))
    (set-window-buffer other elsewhere)
    (align-buffer-integration--settle)
    (align-buffer-integration--keys "C-n C-n C-n")
    (align-buffer-integration--expect
     'a-pane-off-screen "the session survives with one pane shown"
     t (and (align-buffer-session-live-p session) t))
    (align-buffer-integration--expect
     'a-pane-off-screen "and the other buffer was left alone"
     elsewhere (window-buffer other))
    (align-buffer-quit session)
    (kill-buffer elsewhere)))


;;; Running

(defun align-buffer-integration-configure ()
  "Scroll the way a real configuration scrolls."
  (setq-default scroll-conservatively 101)
  (setq-default scroll-margin 5)
  (setq-default scroll-preserve-screen-position t)
  (setq-default auto-window-vscroll nil)
  (setq mouse-wheel-scroll-amount '(1))
  (setq mouse-wheel-progressive-speed nil))

(defun align-buffer-integration-break (what)
  "Put back the behaviour WHAT replaced, so a scenario can be shown to catch it.

A scenario that passes either way tests nothing, so each one names the change it
covers and this is how that claim is checked.  Two of the fixes cannot be checked
this way: the forced paint and the forced redisplay of the follower are both
invisible to assertions on window state, which is the limit of this harness."
  (pcase what
    ("settled-top"
     (defun align-buffer--settled-top (window)
       (align-buffer--viewport-row window)))
    ("defer-timer"
     (defun align-buffer--defer-place (session window side row leader)
       (setq align-buffer--pending-place (list session window side row leader))))
    ("target-recompute"
     (defun align-buffer--apply-pending-place ()
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
                      (eq (window-buffer window)
                          (align-buffer-buffer session side)))
             (let ((align-buffer--syncing t))
               (align-buffer--place-now session window side row leader)))))))
    ("note-point"
     (defun align-buffer--note-placed (window start _point)
       (set-window-parameter window 'align-buffer-placed (cons start nil))))
    ("pane-window"
     (defun align-buffer--pane-window-p (window) (window-live-p window)))
    ("clamp-floor"
     (defun align-buffer--clamp-point-row (row top height margin limit)
       (let ((lowest (+ top margin))
             (highest (- (+ top height) margin 1)))
         (max 0 (min limit (max lowest (min highest row)))))))
    ("placed-flag"
     (defvar align-buffer-integration--flag nil)
     (defun align-buffer--note-placed (_window _start _point)
       (setq align-buffer-integration--flag t))
     (defun align-buffer--own-scroll-p (_window)
       (when align-buffer-integration--flag
         (setq align-buffer-integration--flag nil)
         t)))
    ("can-lead"
     (defun align-buffer--can-lead-p (window)
       (align-buffer--pane-window-p window)))
    (_ (error "Nothing called %s to break" what))))

(defun align-buffer-integration-run ()
  "Run every scenario and return the failures."
  (when-let ((what (getenv "ALIGN_BUFFER_INTEGRATION_BREAK")))
    (dolist (one (split-string what "," t)) (align-buffer-integration-break one)))
  (align-buffer-integration-configure)
  (setq align-buffer-integration--failures nil)
  (dolist (scenario (reverse align-buffer-integration--scenarios))
    (let ((name (car scenario)))
      (condition-case error
          (progn (funcall (cdr scenario))
                 (message "align-buffer integration: %s" name))
        (error (push (format "%s: signalled %s" name
                             (error-message-string error))
                     align-buffer-integration--failures)))))
  align-buffer-integration--failures)

(defun align-buffer-integration--report-file ()
  "Return where to write the report."
  (or (getenv "ALIGN_BUFFER_INTEGRATION_OUT")
      (expand-file-name "align-buffer-integration.txt" temporary-file-directory)))

(defun align-buffer-integration--write (file lines)
  "Write LINES to FILE, one per line."
  (with-temp-file file
    (dolist (line lines) (insert line "\n"))))

(defun align-buffer-integration-batch ()
  "Run the scenarios, write the report, and exit with a status."
  (let ((file (align-buffer-integration--report-file)))
    (run-with-timer
     120 nil
     (lambda ()
       (align-buffer-integration--write file '("TIMED OUT"))
       (kill-emacs 2)))
    (run-with-idle-timer
     0.5 nil
     (lambda ()
       (let* ((failures (align-buffer-integration-run))
              (lines (append
                      (list (format "%d scenarios, %d failure(s)"
                                    (length align-buffer-integration--scenarios)
                                    (length failures)))
                      (reverse failures))))
         (align-buffer-integration--write file lines)
         (kill-emacs (if failures 1 0)))))))

(provide 'align-buffer-integration)

;;; align-buffer-integration.el ends here
