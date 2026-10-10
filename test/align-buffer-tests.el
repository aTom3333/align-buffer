;;; align-buffer-tests.el --- Tests for align-buffer -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;
;;   emacs -Q --batch -L . -l test/align-buffer-tests.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; Batch Emacs never redisplays, so nothing here exercises the scroll hooks or
;; the paint.  What it does cover is the correspondence, the row arithmetic the
;; placement rests on, and where a placement puts a window given a viewport set
;; by hand.  The hooks are the integration harness's job.

;;; Code:

(require 'ert)
(require 'align-buffer)

(defun align-buffer-tests--buffer (name count)
  "Return a fresh buffer called NAME holding COUNT numbered lines."
  (let ((buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (dotimes (index count) (insert (format "line %03d\n" (1+ index))))
      (setq-local truncate-lines t)
      (set-buffer-modified-p nil)
      (goto-char (point-min)))
    buffer))

(defun align-buffer-tests--session (pairs &optional left-count right-count)
  "Return a session over two fresh buffers related by PAIRS."
  (let ((left (align-buffer-tests--buffer "align-left"
                                          (or left-count 200)))
        (right (align-buffer-tests--buffer "align-right"
                                           (or right-count 200))))
    (delete-other-windows)
    (align-buffer-show left right pairs)))

(defmacro align-buffer-tests--with-session (pairs &rest body)
  "Run BODY with `session' bound to a session over PAIRS, then tear it down."
  (declare (indent 1))
  `(let ((session (align-buffer-tests--session ,pairs)))
     (unwind-protect
         (progn ,@body)
       (let ((buffers (list (align-buffer-session-left-buffer session)
                            (align-buffer-session-right-buffer session))))
         (align-buffer-quit session)
         (dolist (buffer buffers)
           (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(defun align-buffer-tests--note (buffer line rows)
  "Hang ROWS screen rows of display above LINE of BUFFER.
A `before-string' on an overlay, the way a checker puts one there: rows of the
display that no position in the buffer stands inside."
  (with-current-buffer buffer
    (let ((overlay (make-overlay (align-buffer-line-position buffer line)
                                 (align-buffer-line-position buffer line))))
      (overlay-put overlay 'before-string
                   (mapconcat (lambda (row) (format "a note, row %d\n" row))
                              (number-sequence 1 rows)
                              ""))
      overlay)))

(defun align-buffer-tests--last-line (buffer)
  "Return the number of BUFFER's last line."
  (with-current-buffer buffer
    (line-number-at-pos (point-max) t)))

(defun align-buffer-tests--window (session side)
  "Return the window showing SESSION's SIDE."
  (get-buffer-window (align-buffer-buffer session side)))

(defun align-buffer-tests--start-line (session side)
  "Return the line SESSION's SIDE window begins at."
  (let ((window (align-buffer-tests--window session side)))
    (align-buffer-line-at (window-buffer window) (window-start window))))

(defun align-buffer-tests--row-of (session side line)
  "Return the screen row LINE sits on in SESSION's SIDE window."
  (let ((window (align-buffer-tests--window session side)))
    (align-buffer--rows-from-start
     window (align-buffer-line-position (window-buffer window) line))))

(defun align-buffer-tests--view (session side line &optional point-line)
  "Put SESSION's SIDE window at LINE, with point on POINT-LINE."
  (let* ((window (align-buffer-tests--window session side))
         (buffer (window-buffer window)))
    (select-window window)
    (set-window-start window (align-buffer-line-position buffer line))
    (align-buffer--set-point
     window (align-buffer-line-position buffer (or point-line line)))))


;;; Building the correspondence

(ert-deftest align-buffer-test-index-is-sorted-by-its-own-line ()
  "An index orders pairs by the line of the side it answers for."
  (let ((index (align-buffer-build-index '((9 . 1) (2 . 5) (7 . 3)) 'left)))
    (should (equal (append index nil) '((2 . 5) (7 . 3) (9 . 1))))))

(ert-deftest align-buffer-test-the-right-index-swaps-the-pairs ()
  "Asked from the right, a pair reads as (RIGHT-LINE . LEFT-LINE)."
  (let ((index (align-buffer-build-index '((9 . 1) (2 . 5) (7 . 3)) 'right)))
    (should (equal (append index nil) '((1 . 9) (3 . 7) (5 . 2))))))

(ert-deftest align-buffer-test-a-vector-of-pairs-is-accepted ()
  "Pairs may arrive as a vector as readily as a list."
  (let ((index (align-buffer-build-index [(3 . 4) (1 . 2)] 'left)))
    (should (equal (append index nil) '((1 . 2) (3 . 4))))))

(ert-deftest align-buffer-test-a-pair-must-be-two-line-numbers ()
  "Anything that is not a cons of two positive integers is refused."
  (should-error (align-buffer-check-pairs '((1 . 2) (3))))
  (should-error (align-buffer-check-pairs '((0 . 2))))
  (should-error (align-buffer-check-pairs '((1 . "2"))))
  (should-not (align-buffer-check-pairs '((1 . 2) (3 . 4)))))


;;; Asking the correspondence

(ert-deftest align-buffer-test-the-anchor-is-the-pair-at-or-below ()
  "A line between two pairs is governed by the one above it."
  (align-buffer-tests--with-session '((10 . 10) (20 . 40) (30 . 50))
    (should (equal (align-buffer-anchor session 'left 25) '(20 . 40)))
    (should (equal (align-buffer-anchor session 'left 20) '(20 . 40)))
    (should (equal (align-buffer-anchor session 'left 999) '(30 . 50)))))

(ert-deftest align-buffer-test-a-line-above-every-pair-gets-the-first ()
  "Below the first pair there is nothing to govern a line, so the first stands in.
Its own line reads as above the asked line, which is how a caller tells."
  (align-buffer-tests--with-session '((10 . 10) (20 . 40))
    (let ((anchor (align-buffer-anchor session 'left 3)))
      (should (equal anchor '(10 . 10)))
      (should (> (car anchor) 3)))
    (should-not (align-buffer-corresponding-line session 'left 3))))

(ert-deftest align-buffer-test-a-correspondence-may-be-empty ()
  "With no pairs at all there is no anchor and nothing corresponds."
  (align-buffer-tests--with-session nil
    (should-not (align-buffer-anchor session 'left 5))
    (should-not (align-buffer-corresponding-line session 'left 5))))

(ert-deftest align-buffer-test-the-last-pair-given-for-a-line-decides ()
  "Where several pairs share a line, the one given last governs it."
  (align-buffer-tests--with-session '((10 . 10) (10 . 77))
    (should (equal (align-buffer-corresponding-line session 'left 10) 77))))

(ert-deftest align-buffer-test-many-lines-may-share-a-counterpart ()
  "A line of one side answering for several of the other is a correspondence
like any other, asked in either direction."
  (align-buffer-tests--with-session '((10 . 5) (11 . 5) (12 . 5))
    (should (equal (align-buffer-corresponding-line session 'left 11) 5))
    (should (equal (align-buffer-corresponding-line session 'right 5) 12))))

(ert-deftest align-buffer-test-a-correspondence-need-not-be-monotonic ()
  "A moved block pairs with lines far away, and both sides still answer."
  (align-buffer-tests--with-session '((10 . 90) (11 . 91) (50 . 20))
    (should (equal (align-buffer-corresponding-line session 'left 10) 90))
    (should (equal (align-buffer-corresponding-line session 'left 50) 20))
    (should (equal (align-buffer-corresponding-line session 'right 90) 10))
    (should (equal (align-buffer-corresponding-line session 'right 20) 50))))

(ert-deftest align-buffer-test-replacing-the-correspondence-changes-the-answers ()
  "A session's pairs can be replaced without building it again."
  (align-buffer-tests--with-session '((10 . 10))
    (should (equal (align-buffer-corresponding-line session 'left 10) 10))
    (align-buffer-set-pairs session '((10 . 60)))
    (should (equal (align-buffer-corresponding-line session 'left 10) 60))))


;;; Lines and positions

(ert-deftest align-buffer-test-a-line-past-the-end-gives-the-last ()
  "A correspondence left stale by an edit still places a window somewhere."
  (let ((buffer (align-buffer-tests--buffer "align-short" 5)))
    (unwind-protect
        (should (equal (align-buffer-line-position buffer 500)
                       (with-current-buffer buffer (point-max))))
      (kill-buffer buffer))))

(ert-deftest align-buffer-test-narrowing-does-not-move-a-line ()
  "Line numbers are absolute, so a reader's narrowing does not shift them."
  (let ((buffer (align-buffer-tests--buffer "align-narrow" 50)))
    (unwind-protect
        (let ((wide (align-buffer-line-position buffer 30)))
          (with-current-buffer buffer
            (narrow-to-region (align-buffer-line-position buffer 20)
                              (align-buffer-line-position buffer 40)))
          (should (equal (align-buffer-line-position buffer 30) wide))
          (should (equal (align-buffer-line-at buffer wide) 30)))
      (kill-buffer buffer))))


;;; Row arithmetic

(ert-deftest align-buffer-test-rows-are-counted-from-the-window-start ()
  "The row of a position is how many screen lines below the start it sits."
  (align-buffer-tests--with-session '((1 . 1))
    (align-buffer-tests--view session 'left 50)
    (should (equal (align-buffer-tests--row-of session 'left 50) 0))
    (should (equal (align-buffer-tests--row-of session 'left 57) 7))))

(ert-deftest align-buffer-test-a-row-above-the-start-is-not-a-row ()
  "Nothing above the window has a row in it, and the count says so."
  (align-buffer-tests--with-session '((1 . 1))
    (align-buffer-tests--view session 'left 50)
    (should-not (align-buffer-tests--row-of session 'left 40))))

(ert-deftest align-buffer-test-a-row-below-the-window-is-not-a-row ()
  "A position further down than the window is tall is refused, which is what
keeps the count from walking a whole buffer."
  (align-buffer-tests--with-session '((1 . 1))
    (align-buffer-tests--view session 'left 1)
    (should-not (align-buffer-tests--row-of session 'left 180))))

(ert-deftest align-buffer-test-a-row-is-counted-the-way-it-is-walked ()
  "A truncated line wider than the window takes one row, and the row of a
position below it is one a walk of that many rows reaches."
  (align-buffer-tests--with-session '((1 . 1))
    (let* ((window (align-buffer-tests--window session 'left))
           (buffer (window-buffer window)))
      (with-current-buffer buffer
        (setq-local truncate-lines t)
        (save-excursion
          (goto-char (align-buffer-line-position buffer 55))
          (end-of-line)
          (insert (make-string (* 3 (window-body-width window)) ?x))))
      (align-buffer-tests--view session 'left 50)
      (let* ((position (align-buffer-line-position buffer 60))
             (row (align-buffer--rows-from-start window position)))
        (should (equal row 10))
        (should (equal (car (align-buffer--position-rows-below
                             window (window-start window) row))
                       position))))))

(ert-deftest align-buffer-test-an-overlay-adds-rows-to-the-count ()
  "Rows an overlay hangs above a line are rows, and the count has them in it."
  (align-buffer-tests--with-session '((1 . 1))
    (let* ((window (align-buffer-tests--window session 'left))
           (buffer (window-buffer window)))
      (align-buffer-tests--view session 'left 50)
      (let ((before (align-buffer--rows-from-start
                     window (align-buffer-line-position buffer 55))))
        (align-buffer-tests--note buffer 52 2)
        (should (equal (align-buffer--rows-from-start
                        window (align-buffer-line-position buffer 55))
                       (+ before 2)))))))

(ert-deftest align-buffer-test-a-start-promises-the-rows-it-gives ()
  "Whatever start comes back, the rows it is said to give are the rows it gives."
  (align-buffer-tests--with-session '((1 . 1))
    (let* ((window (align-buffer-tests--window session 'left))
           (buffer (window-buffer window)))
      (align-buffer-tests--note buffer 52 2)
      (align-buffer-tests--view session 'left 50)
      (let ((position (align-buffer-line-position buffer 60)))
        (dolist (rows (number-sequence 0 12))
          (pcase-let ((`(,start . ,given)
                       (align-buffer--start-for-rows window position rows)))
            (should (equal given (align-buffer--rows-between
                                  window start position)))))))))

(ert-deftest align-buffer-test-a-settled-viewport-needs-no-correction ()
  "Point well inside the window is a window redisplay will leave alone."
  (align-buffer-tests--with-session '((1 . 1))
    (let ((window (align-buffer-tests--window session 'left)))
      (with-current-buffer (window-buffer window) (setq-local scroll-margin 5))
      (align-buffer-tests--view session 'left 50 60)
      (should (equal (align-buffer--settling-correction window) 0)))))

(ert-deftest align-buffer-test-a-viewport-corrects-for-a-point-below-it ()
  "Point past the bottom margin is one redisplay scrolls forward to clear."
  (align-buffer-tests--with-session '((1 . 1))
    (let* ((window (align-buffer-tests--window session 'left))
	   (height (window-body-height window)))
      (with-current-buffer (window-buffer window) (setq-local scroll-margin 5))
      (align-buffer-tests--view session 'left 50 (+ 50 height -1))
      (should (equal (align-buffer--settling-correction window) 5)))))

(ert-deftest align-buffer-test-a-viewport-corrects-for-a-point-above-it ()
  "Point inside the top margin is one redisplay scrolls back to clear."
  (align-buffer-tests--with-session '((1 . 1))
    (let ((window (align-buffer-tests--window session 'left)))
      (with-current-buffer (window-buffer window) (setq-local scroll-margin 5))
      (align-buffer-tests--view session 'left 50 52)
      (should (equal (align-buffer--settling-correction window) -3)))))

(ert-deftest align-buffer-test-the-first-line-has-nothing-to-scroll-back-into ()
  "At the top of the buffer the margin asks for no correction, however close
to the top point sits."
  (align-buffer-tests--with-session '((1 . 1))
    (let ((window (align-buffer-tests--window session 'left)))
      (with-current-buffer (window-buffer window) (setq-local scroll-margin 5))
      (align-buffer-tests--view session 'left 1 2)
      (should (equal (align-buffer--settling-correction window) 0)))))

(ert-deftest align-buffer-test-the-last-line-has-nothing-to-scroll-into ()
  "At the end of the buffer the margin asks for no correction, however close
to the bottom point sits."
  (align-buffer-tests--with-session '((1 . 1))
    (let* ((window (align-buffer-tests--window session 'left))
           (buffer (window-buffer window))
           (last (align-buffer-tests--last-line buffer))
           (height (window-body-height window)))
      (with-current-buffer buffer (setq-local scroll-margin 5))
      ;; The last line on the window's bottom row, so the margin is breached
      ;; and there is nothing below to clear it with.
      (align-buffer-tests--view session 'left (- last height -1) last)
      (should (equal (align-buffer--settling-correction window) 0)))))

(ert-deftest align-buffer-test-a-correction-back-stops-at-the-first-line ()
  "A window two rows into the buffer can only be scrolled back those two."
  (align-buffer-tests--with-session '((1 . 1))
    (let ((window (align-buffer-tests--window session 'left)))
      (with-current-buffer (window-buffer window) (setq-local scroll-margin 5))
      (align-buffer-tests--view session 'left 3 4)
      (should (equal (align-buffer--settling-correction window) -2)))))

(ert-deftest align-buffer-test-a-point-row-inside-the-band-is-left-alone ()
  (should (equal (align-buffer--clamp-point-row 10 nil 30 5) 10)))

(ert-deftest align-buffer-test-a-point-row-clears-the-top-margin ()
  (should (equal (align-buffer--clamp-point-row 2 nil 30 5) 5)))

(ert-deftest align-buffer-test-a-point-row-clears-the-bottom-margin ()
  (should (equal (align-buffer--clamp-point-row 29 nil 30 5) 24)))

(ert-deftest align-buffer-test-a-point-row-at-the-top-of-the-buffer-keeps-its-row ()
  "No floor when the window starts at the first line, so a viewport at the top
does not drag point down past its margin."
  (should (equal (align-buffer--clamp-point-row 0 t 30 5) 0)))

(ert-deftest align-buffer-test-a-point-row-never-leaves-the-window ()
  "A window shorter than twice its margin still gets a row inside itself."
  (should (<= 0 (align-buffer--clamp-point-row 10 nil 4 5) 3)))

(ert-deftest align-buffer-test-a-start-with-room-below-it-is-kept ()
  "Away from the end of the buffer the wanted start is the one used."
  (align-buffer-tests--with-session '((1 . 1))
    (let* ((window (align-buffer-tests--window session 'left))
           (buffer (window-buffer window))
           (start (align-buffer-line-position buffer 50)))
      (should (equal (align-buffer--start-leaving-room window start 5) start)))))

(ert-deftest align-buffer-test-a-start-at-the-end-moves-back-for-the-point-row ()
  "The end of the buffer leaves too few rows, so the start gives up its own."
  (align-buffer-tests--with-session '((1 . 1))
    (let* ((window (align-buffer-tests--window session 'left))
           (buffer (window-buffer window))
           (last (align-buffer-tests--last-line buffer))
           (start (align-buffer-line-position buffer last))
           (moved (align-buffer--start-leaving-room window start 5)))
      (should (< moved start)))))

(ert-deftest align-buffer-test-a-start-moved-back-leaves-the-point-row-reachable ()
  "Wherever the start was asked for, the point row is on offer afterwards."
  (align-buffer-tests--with-session '((1 . 1))
    (let* ((window (align-buffer-tests--window session 'left))
           (buffer (window-buffer window))
           (last (align-buffer-tests--last-line buffer)))
      (dolist (line (list 1 50 (- last 3) (1- last) last))
        (let* ((start (align-buffer--start-leaving-room
                       window (align-buffer-line-position buffer line) 5))
               (reached (cdr (align-buffer--position-rows-below window start 5))))
          (should (equal reached 5)))))))


;;; Placing the other window

(ert-deftest align-buffer-test-the-governing-pair-lands-level ()
  "The pair governing the line point is on sits on one row in both windows."
  (align-buffer-tests--with-session '((10 . 10) (40 . 120))
    (align-buffer-tests--view session 'left 30 45)
    (align-buffer-sync (align-buffer-tests--window session 'left))
    (should (equal (align-buffer-tests--row-of session 'left 40)
		   (align-buffer-tests--row-of session 'right 120)))))

(ert-deftest align-buffer-test-a-moved-block-makes-the-other-window-jump ()
  "A non-monotonic pair is followed like any other, however far it reaches."
  (align-buffer-tests--with-session '((10 . 10) (50 . 150))
    (align-buffer-tests--view session 'left 45 55)
    (align-buffer-sync (align-buffer-tests--window session 'left))
    (should (equal (align-buffer-tests--row-of session 'left 50)
		   (align-buffer-tests--row-of session 'right 150)))))

(ert-deftest align-buffer-test-an-unpaired-run-holds-the-other-window ()
  "Reading a block with no counterpart leaves the other window at the last
line that had one, rather than scrolling it past content that answers nothing."
  (align-buffer-tests--with-session '((10 . 10) (90 . 11))
    ;; Point deep inside the unpaired run, with its governing pair scrolled
    ;; off the top.
    (align-buffer-tests--view session 'left 60 70)
    (align-buffer-sync (align-buffer-tests--window session 'left))
    (should (equal (align-buffer-tests--start-line session 'right) 10))))

(ert-deftest align-buffer-test-the-other-window-stops-rather-than-drifting ()
  "Scrolling further through an unpaired run does not move the other window on."
  (align-buffer-tests--with-session '((10 . 10) (90 . 11))
    (align-buffer-tests--view session 'left 40 50)
    (align-buffer-sync (align-buffer-tests--window session 'left))
    (let ((first (align-buffer-tests--start-line session 'right)))
      (align-buffer-tests--view session 'left 70 80)
      (align-buffer-sync (align-buffer-tests--window session 'left))
      (should (equal (align-buffer-tests--start-line session 'right) first)))))

(ert-deftest align-buffer-test-either-side-can-lead ()
  "The window the reader is in leads, whichever of the two that is."
  (align-buffer-tests--with-session '((10 . 10) (60 . 130))
    (align-buffer-tests--view session 'right 120 135)
    (align-buffer-sync (align-buffer-tests--window session 'right))
    (should (equal (align-buffer-tests--row-of session 'right 130)
		   (align-buffer-tests--row-of session 'left 60)))))

(ert-deftest align-buffer-test-a-window-we-placed-is-not-a-scroll-to-follow ()
  "Rule 4: the follower sits where we put it, so its own hook declines to lead."
  (align-buffer-tests--with-session '((10 . 10) (40 . 120))
    (align-buffer-tests--view session 'left 30 45)
    (align-buffer-sync (align-buffer-tests--window session 'left))
    (should (align-buffer--own-scroll-p
	     (align-buffer-tests--window session 'right)))))

(ert-deftest align-buffer-test-a-reader-moving-the-follower-is-not-our-scroll ()
  "A follower the reader then scrolls stops reading as ours."
  (align-buffer-tests--with-session '((10 . 10) (40 . 120))
    (align-buffer-tests--view session 'left 30 45)
    (align-buffer-sync (align-buffer-tests--window session 'left))
    (let ((follower (align-buffer-tests--window session 'right)))
      (set-window-start follower (align-buffer-line-position
				  (window-buffer follower) 7))
      (should-not (align-buffer--own-scroll-p follower)))))

(ert-deftest align-buffer-test-nothing-corresponding-leaves-the-other-alone ()
  "With no pairs there is nothing to be level with, so no window is moved."
  (align-buffer-tests--with-session nil
    (align-buffer-tests--view session 'right 80)
    (align-buffer-tests--view session 'left 30 40)
    (should-not (align-buffer-sync (align-buffer-tests--window session 'left)))
    (should (equal (align-buffer-tests--start-line session 'right) 80))))


;;; The session

(ert-deftest align-buffer-test-both-buffers-take-part ()
  "Each buffer knows its session and which side of it it is."
  (align-buffer-tests--with-session '((1 . 1))
    (dolist (side '(left right))
      (with-current-buffer (align-buffer-buffer session side)
	(should align-buffer-mode)
	(should (eq (align-buffer-current-session) session))
	(should (eq (align-buffer-side) side))))))

(ert-deftest align-buffer-test-a-session-is-registered-while-it-lives ()
  (align-buffer-tests--with-session '((1 . 1))
    (should (memq session (align-buffer-sessions)))))

(ert-deftest align-buffer-test-a-buffer-belongs-to-one-session-at-a-time ()
  "Scrolling a buffer to two others at once has no meaning, so it is refused."
  (align-buffer-tests--with-session '((1 . 1))
    (let ((other (align-buffer-tests--buffer "align-other" 20)))
      (unwind-protect
	  (should-error
	   (align-buffer-show (align-buffer-session-left-buffer session)
			      other '((1 . 1)))
	   :type 'user-error)
	(kill-buffer other)))))

(ert-deftest align-buffer-test-a-buffer-is-not-shown-against-itself ()
  (let ((buffer (align-buffer-tests--buffer "align-alone" 20)))
    (unwind-protect
        (should-error (align-buffer-show buffer buffer '((1 . 1))))
      (kill-buffer buffer))))

(ert-deftest align-buffer-test-quitting-leaves-both-buffers-alive ()
  "The buffers are the reader's, so a session ending does not take them with it."
  (let* ((session (align-buffer-tests--session '((1 . 1))))
         (left (align-buffer-session-left-buffer session))
         (right (align-buffer-session-right-buffer session)))
    (unwind-protect
        (progn
          (align-buffer-quit session)
          (should (buffer-live-p left))
          (should (buffer-live-p right))
          (dolist (buffer (list left right))
            (with-current-buffer buffer
              (should-not align-buffer-mode)
              (should-not (align-buffer-current-session))))
          (should-not (memq session (align-buffer-sessions))))
      (dolist (buffer (list left right))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest align-buffer-test-quitting-gives-back-the-point-it-moved ()
  "Point goes back where it was in the buffer the reader is not standing in."
  (let* ((left (align-buffer-tests--buffer "align-left" 200))
         (right (align-buffer-tests--buffer "align-right" 200))
         session)
    (unwind-protect
        (progn
          (with-current-buffer left
            (goto-char (align-buffer-line-position left 7)))
          (delete-other-windows)
          (setq session (align-buffer-show left right '((10 . 10) (80 . 90))))
          (select-window (get-buffer-window right))
          (align-buffer-tests--view session 'right 85 95)
          (align-buffer-sync (get-buffer-window right))
          (align-buffer-quit session)
          (should (equal (align-buffer-line-at left (with-current-buffer left
                                                      (point)))
                         7)))
      (dolist (buffer (list left right))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest align-buffer-test-killing-one-buffer-ends-the-session ()
  "A session cannot outlive a buffer, and the other buffer survives it."
  (let* ((session (align-buffer-tests--session '((1 . 1))))
         (left (align-buffer-session-left-buffer session))
         (right (align-buffer-session-right-buffer session)))
    (unwind-protect
        (progn
          (kill-buffer left)
          (should-not (memq session (align-buffer-sessions)))
          (should (buffer-live-p right))
          (with-current-buffer right (should-not align-buffer-mode)))
      (when (buffer-live-p right) (kill-buffer right)))))

(ert-deftest align-buffer-test-a-session-can-own-buffers ()
  "A consumer that made a buffer to show names it, and it goes at teardown."
  (let* ((left (align-buffer-tests--buffer "align-owned" 20))
         (right (align-buffer-tests--buffer "align-right" 20)))
    (delete-other-windows)
    (align-buffer-quit (align-buffer-show left right '((1 . 1))
                                          (list :owned-buffers (list left))))
    (should-not (buffer-live-p left))
    (should (buffer-live-p right))
    (kill-buffer right)))

(ert-deftest align-buffer-test-an-owned-buffer-with-edits-survives ()
  "A buffer the reader has changed is theirs, whoever named it."
  (let* ((left (align-buffer-tests--buffer "align-owned" 20))
         (right (align-buffer-tests--buffer "align-right" 20)))
    (delete-other-windows)
    (let ((session (align-buffer-show left right '((1 . 1))
                                      (list :owned-buffers (list left)))))
      (with-current-buffer left (insert "edited"))
      (align-buffer-quit session))
    (should (buffer-live-p left))
    (dolist (buffer (list left right))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest align-buffer-test-the-post-place-hook-runs-on-a-placement ()
  "A placement that moved the window says so, naming the window and its side."
  (let ((placed nil))
    (let ((align-buffer-post-place-functions
           (list (lambda (session window side)
                   (push (list session window side) placed)))))
      (align-buffer-tests--with-session '((1 . 1) (40 . 40))
        (let ((window (align-buffer-tests--window session 'right)))
          (align-buffer-tests--view session 'left 20 40)
          (setq placed nil)
          (align-buffer-sync (align-buffer-tests--window session 'left))
          (should (equal (length placed) 1))
          (pcase-let ((`(,given-session ,given-window ,given-side)
                       (car placed)))
            (should (eq given-session session))
            (should (eq given-window window))
            (should (eq given-side 'right))))))))

(ert-deftest align-buffer-test-the-post-place-hook-stays-quiet-when-nothing-moved ()
  "A window already where it belongs is not a placement, so nothing is run."
  (let ((placed 0))
    (let ((align-buffer-post-place-functions
           (list (lambda (_session _window _side) (setq placed (1+ placed))))))
      (align-buffer-tests--with-session '((1 . 1) (40 . 40))
        (align-buffer-tests--view session 'left 20 40)
        (align-buffer-sync (align-buffer-tests--window session 'left))
        (setq placed 0)
        (align-buffer-sync (align-buffer-tests--window session 'left))
        (should (equal placed 0))))))

(ert-deftest align-buffer-test-the-hooks-run ()
  (let ((laid-out nil)
        (torn-down nil))
    (let ((align-buffer-post-layout-functions
           (list (lambda (session windows) (setq laid-out (cons session windows)))))
          (align-buffer-pre-teardown-functions
           (list (lambda (session) (setq torn-down session)))))
      (align-buffer-tests--with-session '((1 . 1))
	(should (eq (car laid-out) session))
	(should (= (length (cdr laid-out)) 2)))
      (should torn-down))))

(provide 'align-buffer-tests)

;;; align-buffer-tests.el ends here
