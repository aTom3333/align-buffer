;;; align-buffer-integration.el --- Scroll sync against a real display -*- lexical-binding: t; -*-

;;; Commentary:

;; Batch Emacs never redisplays, so `window-scroll-functions' never fires there
;; and the scroll sync cannot be tested at all.  These scenarios therefore run
;; in a graphical Emacs, driving REAL input through `execute-kbd-macro' so that
;; the command loop and its hooks run for themselves, and synthesising wheel
;; events as `(wheel-down POSITION 1)' vectors.
;;
;; Two things this cannot do.  It cannot see paint: every assertion here reads
;; window state, and a window whose start is right while its pixels are stale
;; looks perfect from Lisp.  And it cannot test what a config contributes, since
;; it runs with -Q; evil, hydra and the rest are absent.
;;
;; The scroll configuration below is not decoration.  With the -Q default of
;; `scroll-conservatively' at 0, Emacs recentres instead of scrolling by a line,
;; and the whole class of bug these scenarios exist for lives in the
;; single-line path.  A run on the defaults quietly tests a different Emacs.
;;
;; THE INVARIANT.  Everything asserts one thing: the pair governing the line
;; point is on in the leader sits on the same screen row in both windows.  When
;; that pair has scrolled off the leader's top it has no row there, and the
;; follower is then expected to show its counterpart on row zero, which is what
;; stops an unpaired run from dragging the follower along.
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
  "Record that SCENARIO wanted WANTED for WHAT and got GOT."
  (unless (equal wanted got)
    (push (format "%s: %s: wanted %S, got %S" scenario what wanted got)
          align-buffer-integration--failures)))


;;; Reading the two windows

(defun align-buffer-integration--leader ()
  "Return the selected window when it shows a buffer taking part."
  (let ((window (selected-window)))
    (and (buffer-local-value 'align-buffer--side (window-buffer window))
         window)))

(defun align-buffer-integration--anchor (session leader)
  "Return the pair governing LEADER's point line, or nil when none does."
  (let* ((side (buffer-local-value 'align-buffer--side (window-buffer leader)))
         (line (align-buffer-line-at (window-buffer leader)
                                     (window-point leader)))
         (anchor (and line (align-buffer-anchor session side line))))
    (and anchor (<= (car anchor) line) (cons side anchor))))

(defun align-buffer-integration--row (window line)
  "Return the screen row LINE sits on in WINDOW, or nil when it is not shown."
  (align-buffer--rows-from-start
   window (align-buffer-line-position (window-buffer window) line)))

(defun align-buffer-integration--rows (session)
  "Return (LEADER-ROW . FOLLOWER-ROW) for the governing pair, or nil."
  (let* ((leader (align-buffer-integration--leader))
         (found (and leader (align-buffer-integration--anchor session leader))))
    (when found
      (let* ((side (car found))
             (anchor (cdr found))
             (follower (align-buffer--follower session side)))
        (when (window-live-p follower)
          (cons (align-buffer-integration--row leader (car anchor))
                (align-buffer-integration--row follower (cdr anchor))))))))

(defun align-buffer-integration--level-p (session)
  "Return non-nil when the governing pair sits on one row in both windows."
  (let ((rows (align-buffer-integration--rows session)))
    (and rows (equal (cdr rows) (or (car rows) 0)))))

(defun align-buffer-integration--starts (session)
  "Return the line each of SESSION's windows begins at, as (LEFT . RIGHT)."
  (cons (align-buffer-integration--start session 'left)
        (align-buffer-integration--start session 'right)))

(defun align-buffer-integration--start (session side)
  "Return the line SESSION's SIDE window begins at."
  (let ((window (get-buffer-window (align-buffer-buffer session side))))
    (and (window-live-p window)
         (align-buffer-line-at (window-buffer window) (window-start window)))))


;;; Driving the display

(defun align-buffer-integration--settle ()
  "Let the timers and the redisplay they ask for run."
  (dotimes (_ 3)
    (redisplay t)
    (sit-for 0.05)))

(defun align-buffer-integration--wheel (window direction)
  "Send one wheel notch of DIRECTION over WINDOW, and let it settle."
  (align-buffer-integration--wheel-without-settling window direction)
  (align-buffer-integration--settle))

(defun align-buffer-integration--wheel-without-settling (window direction)
  "Send one wheel notch of DIRECTION over WINDOW."
  (let ((position (list window (window-start window) '(0 . 0) 0)))
    (execute-kbd-macro (vector (list direction position 1)))))

(defun align-buffer-integration--keys (keys)
  "Run KEYS, a `kbd' string, as real input, and let it settle."
  (execute-kbd-macro (kbd keys))
  (align-buffer-integration--settle))

(defun align-buffer-integration--pairs ()
  "Return a correspondence with every shape the sync has to cope with.
Lines 1 to 100 pair across; 101 to 140 of the left have no counterpart; 201 to
220 of the left moved to the end of the right; the rest pairs in order."
  (append
   (cl-loop for line from 1 to 100 collect (cons line line))
   (cl-loop for offset from 0 below 60
            collect (cons (+ 141 offset) (+ 101 offset)))
   (cl-loop for offset from 0 below 20
            collect (cons (+ 201 offset) (+ 280 offset)))
   (cl-loop for offset from 0 below 80
            collect (cons (+ 221 offset) (+ 161 offset)))))

(defun align-buffer-integration--buffer (name count)
  "Return a buffer called NAME holding COUNT numbered lines."
  (let ((buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (dotimes (index count)
        (insert (format "%s line %03d\n" name (1+ index))))
      (setq-local truncate-lines t)
      (set-buffer-modified-p nil)
      (goto-char (point-min)))
    buffer))

(defun align-buffer-integration--session ()
  "Build a session over two real buffers, and return it."
  (delete-other-windows)
  (let ((left (align-buffer-integration--buffer "old" 300))
        (right (align-buffer-integration--buffer "new" 300)))
    (let ((session (align-buffer-show left right
                                      (align-buffer-integration--pairs)
                                      (list :owned-buffers (list left right)))))
      (align-buffer-integration--settle)
      session)))

(defun align-buffer-integration--view (session side line point-line)
  "Put SESSION's SIDE window at LINE with point on POINT-LINE, and let it settle."
  (let* ((window (get-buffer-window (align-buffer-buffer session side)))
         (buffer (window-buffer window)))
    (select-window window)
    (set-window-start window (align-buffer-line-position buffer line))
    (align-buffer--set-point window (align-buffer-line-position buffer point-line))
    (align-buffer-integration--settle)))

(defun align-buffer-integration--note (session side line rows)
  "Hang ROWS screen rows of display above LINE of SESSION's SIDE buffer."
  (let ((buffer (align-buffer-buffer session side)))
    (with-current-buffer buffer
      (let ((overlay (make-overlay (align-buffer-line-position buffer line)
                                   (align-buffer-line-position buffer line))))
        (overlay-put overlay 'before-string
                     (mapconcat (lambda (row) (format "a note, row %d\n" row))
                                (number-sequence 1 rows)
                                "")))
      (set-buffer-modified-p nil))))

(defun align-buffer-integration--heighten (session side line scale)
  "Give LINE of SESSION's SIDE buffer a face SCALE times the usual height."
  (let ((buffer (align-buffer-buffer session side)))
    (with-current-buffer buffer
      (let ((overlay (make-overlay
                      (align-buffer-line-position buffer line)
                      (align-buffer-line-position buffer (1+ line)))))
        (overlay-put overlay 'face (list :height scale)))
      (set-buffer-modified-p nil))))

(defun align-buffer-integration--pixel-y (window line)
  "Return how far below WINDOW's top LINE sits, in pixels, or nil when hidden."
  (let ((posn (posn-at-point
               (align-buffer-line-position (window-buffer window) line)
               window)))
    (and posn (cdr (posn-x-y posn)))))

(defun align-buffer-integration--out-by (session)
  "Return how many pixels apart the governing pair's two lines are drawn."
  (let* ((leader (align-buffer-integration--leader))
         (found (and leader (align-buffer-integration--anchor session leader))))
    (when found
      (let* ((side (car found))
             (anchor (cdr found))
             (follower (align-buffer--follower session side))
             (one (align-buffer-integration--pixel-y leader (car anchor)))
             (other (and (window-live-p follower)
                         (align-buffer-integration--pixel-y
                          follower (cdr anchor)))))
        (and one other (abs (- other one)))))))

(defun align-buffer-integration--lengthen (session line)
  "Make LINE of both of SESSION's buffers wider than the window showing it."
  (dolist (side '(left right))
    (let* ((window (get-buffer-window (align-buffer-buffer session side)))
           (buffer (window-buffer window)))
      (with-current-buffer buffer
        (save-excursion
          (goto-char (align-buffer-line-position buffer line))
          (end-of-line)
          (insert (make-string (* 2 (window-body-width window)) ?x)))
        (set-buffer-modified-p nil)))))

(defun align-buffer-integration--go (session side line)
  "Select SESSION's SIDE window and put point on LINE there, as a command would."
  (let ((window (get-buffer-window (align-buffer-buffer session side))))
    (select-window window)
    (with-current-buffer (window-buffer window)
      (goto-char (align-buffer-line-position (current-buffer) line)))
    (recenter)
    (align-buffer-integration--settle)))



;;; The scenarios

(align-buffer-integration-defscenario held-key
  (let ((session (align-buffer-integration--session)))
    (align-buffer-integration--go session 'left 40)
    (align-buffer-integration--keys "C-n C-n C-n C-n C-n C-n C-n C-n")
    (align-buffer-integration--expect
     'held-key "level after eight line motions"
     t (align-buffer-integration--level-p session))
    (align-buffer-integration--keys "C-p C-p C-p")
    (align-buffer-integration--expect
     'held-key "level after moving back"
     t (align-buffer-integration--level-p session))
    (align-buffer-quit session)))

(align-buffer-integration-defscenario a-page-at-a-time
  (let ((session (align-buffer-integration--session)))
    (align-buffer-integration--go session 'left 20)
    (align-buffer-integration--keys "C-v")
    (align-buffer-integration--expect
     'a-page-at-a-time "level after a page down"
     t (align-buffer-integration--level-p session))
    (align-buffer-integration--keys "M-v")
    (align-buffer-integration--expect
     'a-page-at-a-time "level after a page back"
     t (align-buffer-integration--level-p session))
    (align-buffer-quit session)))

(align-buffer-integration-defscenario wheel-over-the-active-window
  (let ((session (align-buffer-integration--session)))
    (align-buffer-integration--go session 'left 40)
    (dotimes (_ 4)
      (align-buffer-integration--wheel (selected-window) 'wheel-down))
    (align-buffer-integration--expect
     'wheel-over-the-active-window "level after four notches"
     t (align-buffer-integration--level-p session))
    (align-buffer-quit session)))

;;; Fails without the zero-delay timer (`defer-timer'): a notch over the window
;;; the reader is not in runs its command hook before redisplay reaches us, so a
;;; placement left for the next hook waits on the next keystroke.
(align-buffer-integration-defscenario wheel-over-the-inactive-window
  (let ((session (align-buffer-integration--session)))
    (align-buffer-integration--go session 'left 40)
    (let ((follower (get-buffer-window
		     (align-buffer-session-right-buffer session))))
      (align-buffer-integration--wheel follower 'wheel-down)
      (align-buffer-integration--expect
       'wheel-over-the-inactive-window "the other window followed it"
       t (align-buffer-integration--level-p session)))
    (align-buffer-quit session)))

(align-buffer-integration-defscenario wheel-burst-over-the-inactive-window
  (let ((session (align-buffer-integration--session)))
    (align-buffer-integration--go session 'left 40)
    (let ((follower (get-buffer-window
		     (align-buffer-session-right-buffer session))))
      (dotimes (_ 5)
	(align-buffer-integration--wheel-without-settling follower 'wheel-down))
      (align-buffer-integration--settle)
      (align-buffer-integration--expect
       'wheel-burst-over-the-inactive-window "level after the burst"
       t (align-buffer-integration--level-p session)))
    (align-buffer-quit session)))

(align-buffer-integration-defscenario notches-arriving-faster-than-we-place
  (let ((session (align-buffer-integration--session)))
    (align-buffer-integration--go session 'left 40)
    (dotimes (_ 6)
      (align-buffer-integration--wheel-without-settling (selected-window)
							'wheel-down))
    (align-buffer-integration--settle)
    (align-buffer-integration--expect
     'notches-arriving-faster-than-we-place "level once the burst is over"
     t (align-buffer-integration--level-p session))
    (align-buffer-quit session)))

(align-buffer-integration-defscenario a-leader-about-to-be-corrected
  (let ((session (align-buffer-integration--session)))
    (align-buffer-integration--go session 'left 40)
    ;; Point left where the bottom margin will make redisplay scroll the
    ;; window, so a row read before that lands is a row too high.
    (let* ((window (selected-window))
	   (height (window-body-height window)))
      (with-current-buffer (window-buffer window)
	(goto-char (align-buffer-line-position
		    (current-buffer)
		    (+ (align-buffer-line-at (current-buffer)
					     (window-start window))
		       height -1))))
      (align-buffer-integration--settle)
      (align-buffer-integration--expect
       'a-leader-about-to-be-corrected "level once the leader has settled"
       t (align-buffer-integration--level-p session)))
    (align-buffer-quit session)))

;;; Fails without the missing floor at the first line (`clamp-floor'), which
;;; otherwise drags the follower's point a margin down from a viewport at the top.
(align-buffer-integration-defscenario the-top-of-the-buffer-holds-its-point
  (let ((session (align-buffer-integration--session)))
    (align-buffer-integration--go session 'left 1)
    (align-buffer-integration--keys "M-<")
    (align-buffer-integration--expect
     'the-top-of-the-buffer-holds-its-point "both windows start at the first line"
     '(1 . 1) (align-buffer-integration--starts session))
    (align-buffer-integration--expect
     'the-top-of-the-buffer-holds-its-point "the follower's point stayed on row zero"
     1 (let ((follower (get-buffer-window
			(align-buffer-session-right-buffer session))))
	 (align-buffer-line-at (window-buffer follower)
			       (window-point follower))))
    (align-buffer-quit session)))

(align-buffer-integration-defscenario crossing-into-a-moved-block
  (let ((session (align-buffer-integration--session)))
    ;; Left 201 pairs with right 280, which is nowhere near where the lines
    ;; before it pair.
    (align-buffer-integration--go session 'left 195)
    (let ((before (align-buffer-integration--start session 'right)))
      (align-buffer-integration--go session 'left 205)
      (align-buffer-integration--expect
       'crossing-into-a-moved-block "level on the moved block"
       t (align-buffer-integration--level-p session))
      (align-buffer-integration--expect
       'crossing-into-a-moved-block "and the follower did jump"
       t (not (equal before (align-buffer-integration--start session 'right)))))
    (align-buffer-quit session)))

(align-buffer-integration-defscenario back-out-of-a-moved-block
  (let ((session (align-buffer-integration--session)))
    (align-buffer-integration--go session 'left 205)
    (align-buffer-integration--go session 'left 230)
    (align-buffer-integration--expect
     'back-out-of-a-moved-block "level again after the block"
     t (align-buffer-integration--level-p session))
    (align-buffer-quit session)))

;;; Fails without the clamp (`clamp-off-screen-anchor') and without the placement
;;; record answered by value (`placed-flag').
(align-buffer-integration-defscenario an-unpaired-run-holds-the-follower
  (let ((session (align-buffer-integration--session)))
    ;; Left 101 to 140 pair with nothing.  Read deep enough into them that the
    ;; last paired line, 100, has left the top of the leader: the follower is
    ;; then expected to hold that line at its own top rather than scrolling on
    ;; by the height of the run.
    (align-buffer-integration--go session 'left 130)
    (align-buffer-integration--expect
     'an-unpaired-run-holds-the-follower "the follower waits on the last pair"
     100 (align-buffer-integration--start session 'right))
    (align-buffer-integration--go session 'left 138)
    (align-buffer-integration--expect
     'an-unpaired-run-holds-the-follower "and reading further does not move it"
     100 (align-buffer-integration--start session 'right))
    (align-buffer-quit session)))

(align-buffer-integration-defscenario one-notch-on-each-window
  (let ((session (align-buffer-integration--session)))
    (align-buffer-integration--go session 'left 40)
    (let ((left (get-buffer-window (align-buffer-session-left-buffer session)))
	  (right (get-buffer-window (align-buffer-session-right-buffer session))))
      (align-buffer-integration--wheel left 'wheel-down)
      (align-buffer-integration--wheel right 'wheel-down)
      (align-buffer-integration--expect
       'one-notch-on-each-window "level after a notch on each"
       t (align-buffer-integration--level-p session)))
    (align-buffer-quit session)))

;;; Fails without `align-buffer--can-lead-p': `display-buffer' makes a window
;;; before it gives it a buffer, so for one redisplay two windows show the same
;;; buffer with different heights, and following the new one moves the follower
;;; off by their difference.
(align-buffer-integration-defscenario a-transient-window-does-not-lead
  (let ((session (align-buffer-integration--session)))
    (align-buffer-integration--go session 'left 40)
    ;; Only the follower is asserted on.  Splitting the leader halves its
    ;; height, and redisplay rescrolling a window whose point no longer clears
    ;; the margin is correct behaviour rather than something to catch.
    (let ((before (align-buffer-integration--start session 'right))
	  (elsewhere (align-buffer-integration--buffer "elsewhere" 20)))
      (let ((window (split-window (selected-window) nil 'below)))
	(set-window-buffer window (window-buffer (selected-window)))
	(align-buffer-integration--settle)
	(set-window-buffer window elsewhere)
	(align-buffer-integration--settle)
	(when (window-live-p window) (delete-window window)))
      (align-buffer-integration--settle)
      (align-buffer-integration--expect
       'a-transient-window-does-not-lead "the follower was not dragged"
       before (align-buffer-integration--start session 'right))
      (kill-buffer elsewhere))
    (align-buffer-quit session)))

(align-buffer-integration-defscenario a-window-off-screen
  (let ((session (align-buffer-integration--session)))
    (align-buffer-integration--go session 'left 40)
    (let ((elsewhere (align-buffer-integration--buffer "elsewhere" 20))
	  (follower (get-buffer-window
		     (align-buffer-session-right-buffer session))))
      (set-window-buffer follower elsewhere)
      (align-buffer-integration--settle)
      (align-buffer-integration--keys "C-n C-n")
      (align-buffer-integration--expect
       'a-window-off-screen "the session is still alive"
       t (align-buffer-session-live-p session))
      (align-buffer-integration--expect
       'a-window-off-screen "and the other buffer was left alone"
       elsewhere (window-buffer follower))
      (align-buffer-quit session)
      (kill-buffer elsewhere))))

(align-buffer-integration-defscenario quitting-gives-the-window-back
  (let* ((session (align-buffer-integration--session))
	 (left (align-buffer-session-left-buffer session))
	 (right (align-buffer-session-right-buffer session)))
    (align-buffer-integration--go session 'left 40)
    (align-buffer-quit session)
    (align-buffer-integration--settle)
    (align-buffer-integration--expect
     'quitting-gives-the-window-back "one window is left"
     1 (length (window-list)))
    (align-buffer-integration--expect
     'quitting-gives-the-window-back "and both buffers are gone with the session"
     '(nil nil) (list (buffer-live-p left) (buffer-live-p right)))))

;;; Fails without measuring rows as the display makes them (`row-pixels'), and
;;; fails too when the measuring is added up a step at a time (`row-sum').  The
;;; note hangs two rows above the line the follower has to show, and those rows
;;; are display rather than text: `vertical-motion' walks straight past them.
(align-buffer-integration-defscenario a-note-above-the-follower
  (let ((session (align-buffer-integration--session)))
    (align-buffer-integration--note session 'right 30 2)
    (align-buffer-integration--lengthen session 35)
    ;; Centred, so the note and the long line both sit between the follower's
    ;; start and the line it has to show.
    (align-buffer-integration--go session 'left 40)
    (align-buffer-integration--expect
     'a-note-above-the-follower "level with a note above the follower's line"
     t (align-buffer-integration--level-p session))
    (align-buffer-integration--keys "C-n C-n C-n")
    (align-buffer-integration--expect
     'a-note-above-the-follower "and level three lines on"
     t (align-buffer-integration--level-p session))
    (align-buffer-integration--keys "C-p C-p")
    (align-buffer-integration--expect
     'a-note-above-the-follower "and level coming back"
     t (align-buffer-integration--level-p session))
    (align-buffer-quit session)))

;;; Fails without taking the nearer of the two starts (`start-overshoots').  A
;;; line of its own height above the anchor leaves the pixel asked for inside a
;;; row of the other window, where only a row boundary is on offer; the nearer
;;; boundary is half a row out and the one past it a whole row.
(align-buffer-integration-defscenario a-taller-line-above-the-anchor
  (let* ((session (align-buffer-integration--session))
         (window (get-buffer-window (align-buffer-session-left-buffer session)))
         (half (/ (align-buffer--row-height window) 2)))
    (align-buffer-integration--heighten session 'left 40 2.0)
    (align-buffer-integration--go session 'right 50)
    (align-buffer-integration--expect
     'a-taller-line-above-the-anchor "within half a row of level"
     t (<= (align-buffer-integration--out-by session) half))
    (align-buffer-integration--keys "C-n C-n")
    (align-buffer-integration--expect
     'a-taller-line-above-the-anchor "and still within half a row"
     t (<= (align-buffer-integration--out-by session) half))
    ;; Fails on its own without `point-from-pixels': the point the follower is
    ;; given drifts down the buffer by a line for every tall line above it.
    (align-buffer-integration--expect
     'a-taller-line-above-the-anchor "the follower's point is on the pair"
     52 (align-buffer-line-at
         (align-buffer-session-left-buffer session)
         (window-point (get-buffer-window
                        (align-buffer-session-left-buffer session)))))
    (align-buffer-quit session)))

;;; Point past the bottom margin with the end of the buffer already on screen.
;;; Passes with `settling-cap' undone as well: redisplay moves the leader here
;;; whatever the prediction, and a leader that moves fires its scroll hook and
;;; syncs again, so the error repairs itself.  The two tests on
;;; `align-buffer--settling-correction' are what pin the fix.
(align-buffer-integration-defscenario arriving-at-the-end-of-the-buffer
  (let* ((session (align-buffer-integration--session))
         (window (get-buffer-window (align-buffer-session-right-buffer session)))
         (height (window-body-height window))
         (margin (align-buffer-effective-margin window))
         (last (with-current-buffer (window-buffer window)
                 (line-number-at-pos (point-max) t))))
    ;; The last line on the lowest row the margin allows, which is where a
    ;; placement of this window leaves its point.  The end of the buffer is then
    ;; one row further, so a press down breaches the margin with nothing below
    ;; to clear it.
    (align-buffer-integration--view session 'right
                                    (- last (- height margin)) (1- last))
    (align-buffer-integration--expect
     'arriving-at-the-end-of-the-buffer "level on the last line"
     t (align-buffer-integration--level-p session))
    (align-buffer-integration--keys "C-n")
    (align-buffer-integration--expect
     'arriving-at-the-end-of-the-buffer "level after pressing past it"
     t (align-buffer-integration--level-p session))
    (align-buffer-quit session)))

;;; Fails without counting rows by the motion that moves them (`row-count').  A
;;; truncated line wider than the window reads as two rows to
;;; `count-screen-lines' and one to `vertical-motion', so a long line between
;;; the start and the anchor puts the follower a line out.
(align-buffer-integration-defscenario a-long-line-above-the-anchor
  (let ((session (align-buffer-integration--session)))
    (align-buffer-integration--lengthen session 40)
    ;; Centred, so line 40 lies between the start and point.
    (align-buffer-integration--go session 'right 50)
    (align-buffer-integration--keys "C-n")
    (align-buffer-integration--expect
     'a-long-line-above-the-anchor
     "level with a long line above the anchor"
     t (align-buffer-integration--level-p session))
    (align-buffer-integration--keys "C-n C-n")
    (align-buffer-integration--expect
     'a-long-line-above-the-anchor
     "and still level two lines further on"
     t (align-buffer-integration--level-p session))
    (align-buffer-quit session)))

;;; Fails without the room the point row needs (`point-row-room').  The pair
;;; governing the right's unpaired run is its line 240, whose counterpart is the
;;; LAST line of the left, so the follower is asked for a start with nothing
;;; below it and a point the margin forbids.  Redisplay then scrolls the
;;; follower, the follower reads as scrolled by the reader, and it leads.
(align-buffer-integration-defscenario the-end-of-the-follower-does-not-lead
  (let ((session (align-buffer-integration--session))
	;; Recentring, which is what a window
	;; whose margin is breached does on the
	;; -Q default.
	(scroll-conservatively 0))
    (align-buffer-integration--go session 'right 241)
    (dotimes (_ 30)
      (align-buffer-integration--keys "C-n"))
    (align-buffer-integration--expect
     'the-end-of-the-follower-does-not-lead
     "point is where the thirty motions put it"
     271 (align-buffer-line-at
	  (align-buffer-session-right-buffer session)
	  (window-point
	   (get-buffer-window
	    (align-buffer-session-right-buffer session)))))
    (align-buffer-quit session)))


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

A scenario that passes either way tests nothing, so each fix is named here and
the run can be repeated with it undone.  Rule 1 has no entry: the target is
worked out inside the placement itself and there is no cached row left to
stale.

Measured, one run per name.  Eleven of the fifteen are caught:

  defer-timer               wheel-over-the-inactive-window,
                            wheel-burst-over-the-inactive-window,
                            one-notch-on-each-window
  clamp-off-screen-anchor   an-unpaired-run-holds-the-follower,
                            a-taller-line-above-the-anchor
  placed-flag               an-unpaired-run-holds-the-follower,
                            a-note-above-the-follower,
                            a-taller-line-above-the-anchor,
                            arriving-at-the-end-of-the-buffer,
                            the-end-of-the-follower-does-not-lead
  clamp-floor               the-top-of-the-buffer-holds-its-point
  can-lead                  a-transient-window-does-not-lead
  point-row-room            the-end-of-the-follower-does-not-lead
  row-count                 a-long-line-above-the-anchor,
                            a-note-above-the-follower
  row-pixels                a-note-above-the-follower
  row-sum                   a-note-above-the-follower,
                            a-taller-line-above-the-anchor
  start-overshoots          a-taller-line-above-the-anchor
  point-from-pixels         a-taller-line-above-the-anchor

And four are caught by nothing here:

  settling-cap         the bug needs redisplay to leave the leader where it is
                       despite point breaching the margin, which is what it
                       does at the end of a buffer it already shows whole.
                       Every geometry tried here recentres the leader instead,
                       and a leader that moves syncs again off its own scroll
                       hook, so the wrong prediction is repaired before it can
                       be seen.  Observed in a live session on the demo, where
                       the leader held still and the follower moved a row.
                       Covered by `align-buffer-test-the-last-line-has-nothing-
                       to-scroll-into' instead.

  settling-correction  the bug needs redisplay's correction of the leader to
                       arrive while we are inside our own sync, which happens
                       only through the forced redisplay in `post-command-hook'.
                       A scenario settling at top level is outside that guard,
                       so the correction is simply followed.  Recorded against
                       the same fix in the synthetic-pane version, where the
                       evidence was a log from a live session.
  note-point           a point-motion scroll whose window start is still stale
                       when the command hook runs.  Not reached by this set.
  window-big-enough    wants a window of one or two rows, which this harness
                       has no way to lay out."
  (pcase what
    ("settling-correction"
     (defun align-buffer--settling-correction (_window) 0))
    ("clamp-off-screen-anchor"
     ;; The unclamped reading, which lets a pair above the leader's top take a
     ;; negative offset and so drags the follower on by the height of the run.
     ;; Both halves are needed: the clamp is read twice, once where the offset
     ;; is worked out and once where it is turned into a start.
     (defun align-buffer--leader-pixels (window position)
       (let ((start (window-start window)))
         (if (>= position start)
             (- (align-buffer--pixels-between window start position)
                (* (align-buffer--settling-correction window)
                   (align-buffer--row-height window)))
           (- (align-buffer--pixels-between window position start)))))
     (defun align-buffer--target (session side leader follower)
       (let* ((leader-buffer (window-buffer leader))
              (line (align-buffer-line-at leader-buffer (window-point leader)))
              (anchor (and line (align-buffer-anchor session side line))))
         (when anchor
           (let* ((height (max 1 (window-body-height follower)))
                  (row-height (align-buffer--row-height follower))
                  (other-position (align-buffer-line-position
                                   (window-buffer follower) (cdr anchor)))
                  (pixels (if (> (car anchor) line)
                              0
                            (align-buffer--leader-pixels
                             leader
                             (align-buffer-line-position
                              leader-buffer (car anchor))))))
             (when other-position
               (cons other-position
                     (min (* (1- height) row-height) pixels)))))))
     (defun align-buffer--start-for-pixels (window position pixels)
       (if (zerop pixels)
           (cons position 0)
         (let ((rows (round pixels (align-buffer--row-height window))))
           (let ((start (car (align-buffer--position-rows-below
                              window position (- rows)))))
             (cons start (align-buffer--pixels-between
                          window start position)))))))
    ("defer-timer"
     (defun align-buffer--defer-place (session window side leader)
       (setq align-buffer--pending-place (list session window side leader))))
    ("note-point"
     (defun align-buffer--note-placed (window start _point)
       (set-window-parameter window 'align-buffer-placed (cons start nil))))
    ("window-big-enough"
     (defun align-buffer--window-big-enough-p (window) (window-live-p window)))
    ("clamp-floor"
     (defun align-buffer--clamp-point-row (row _start-at-beginning height margin)
       (let ((lowest margin)
             (highest (max 0 (- height margin 1))))
         (max 0 (min (1- height) (max lowest (min highest row)))))))
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
       (align-buffer--window-big-enough-p window)))
    ("point-row-room"
     (defun align-buffer--start-leaving-room (_window start _rows) start))
    ("row-pixels"
     ;; Rows counted by walking the text, which is blind to the rows an overlay
     ;; hangs above a line.
     (defun align-buffer--rows-from-start (window position)
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
                       (setq answer row)
                     (cond ((> (point) position) (setq answer row))
                           ((= (point) position) (setq answer (1+ row)))
                           (t (setq row (1+ row))))))
                 (when (and answer (< answer height)) answer))))))))
    ("row-sum"
     ;; The measuring added up a step at a time, which comes to a row more than
     ;; the span is worth wherever a step lands inside a wrapped line.
     (defun align-buffer--start-for-pixels (window position pixels)
       (if (<= pixels 0)
           (cons position 0)
         (with-current-buffer (window-buffer window)
           (save-excursion
             (goto-char position)
             (let ((best position) (best-pixels 0) (sum 0) (done nil))
               (while (not done)
                 (let ((below (point)))
                   (if (zerop (vertical-motion -1 window))
                       (setq done t)
                     (setq sum (+ sum (align-buffer--pixels-between
                                       window (point) below)))
                     (setq best (point))
                     (setq best-pixels sum)
                     (when (>= sum pixels) (setq done t)))))
               (cons best (max 0 (- best-pixels pixels)))))))))
    ("point-from-pixels"
     ;; The follower's point put on a row worked out by dividing the pixels by
     ;; the usual row height, which every line drawn taller than a row inflates,
     ;; so point drifts a line further down the pair for each of them.
     (defun align-buffer--rows-apart (window from to)
       (round (align-buffer--pixels-between window from to)
              (align-buffer--row-height window)))
     (defun align-buffer--follower-point (window start _counterpart _row
                                                 point-row)
       (car (align-buffer--position-rows-below window start point-row))))
    ("start-overshoots"
     ;; The first start past what was asked rather than the nearer of the two,
     ;; which leaves the follower a row low wherever the pixel falls inside a
     ;; row.
     (defun align-buffer--start-for-pixels (window position pixels)
       (if (<= pixels 0)
           (cons position 0)
         (with-current-buffer (window-buffer window)
           (save-excursion
             (goto-char position)
             (let ((best position) (best-pixels 0) (done nil))
               (while (not done)
                 (if (zerop (vertical-motion -1 window))
                     (setq done t)
                   (setq best (point))
                   (setq best-pixels (align-buffer--pixels-between
                                      window (point) position))
                   (when (>= best-pixels pixels) (setq done t))))
               (cons best best-pixels)))))))
    ("settling-cap"
     ;; The correction as predicted, with no account of whether the window has
     ;; anywhere to scroll.
     (defun align-buffer--settling-correction (window)
       (let* ((height (max 1 (window-body-height window)))
              (margin (align-buffer-effective-margin window))
              (point-row (align-buffer--rows-from-start
                          window (window-point window)))
              (highest (max 0 (- height margin 1)))
              (at-beginning (with-current-buffer (window-buffer window)
                              (= (window-start window) (point-min)))))
         (cond ((null point-row) 0)
               ((> point-row highest) (- point-row highest))
               ((and (< point-row margin) (not at-beginning))
                (- point-row margin))
               (t 0)))))
    ("row-count"
     ;; Rows counted by `count-screen-lines' while they are walked by
     ;; `vertical-motion', which disagree over a truncated long line.
     (defun align-buffer--rows-from-start (window position)
       (let ((start (window-start window))
             (height (max 1 (window-body-height window))))
         (when (>= position start)
           (let ((bound (car (align-buffer--position-rows-below
                              window start height))))
             (when (or (<= position bound)
                       (= bound (with-current-buffer (window-buffer window)
                                  (point-max))))
               (min (1- height)
                    (count-screen-lines start position nil window))))))))
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
                     align-buffer-integration--failures)))
      ;; A scenario that failed part way can leave a session standing, and the
      ;; next one starts by deleting windows.
      (dolist (session (align-buffer-sessions))
        (align-buffer-quit session))))
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
     180 nil
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
