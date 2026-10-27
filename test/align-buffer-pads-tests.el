;;; align-buffer-pads-tests.el --- Tests for the pad rows -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   emacs -Q --batch -L . -l test/align-buffer-pads-tests.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; The pads are put in by hand here, the way a consumer puts them in, so what
;; is tested is the rows and the walk rather than whatever worked out that a
;; run of lines was missing.

;;; Code:

(require 'ert)
(require 'align-buffer)

(defface align-buffer-pads-tests-face '((t :extend t))
  "A face standing in for whatever colour a consumer draws its rows in.")

(defmacro align-buffer-pads-tests--with-session (left-text right-text pairs
                                                           &rest body)
  "Show LEFT-TEXT against RIGHT-TEXT, related by PAIRS, and run BODY.
`left', `right' and `session' are bound, and both buffers walk their pads."
  (declare (indent 3))
  `(let ((left (generate-new-buffer " *align-buffer pads left*"))
         (right (generate-new-buffer " *align-buffer pads right*")))
     (unwind-protect
         (progn
           (with-current-buffer left (insert ,left-text))
           (with-current-buffer right (insert ,right-text))
           (delete-other-windows)
           (let ((session (align-buffer-show left right ,pairs)))
             (unwind-protect
                 (progn (dolist (buffer (list left right))
                          (with-current-buffer buffer
                            (align-buffer-navigation-mode 1)))
                        ,@body)
               (align-buffer-quit session))))
       (kill-buffer left)
       (kill-buffer right))))

(defun align-buffer-pads-tests--pad (buffer line rows &optional facing
                                            facing-line)
  "Hang ROWS rows above LINE of BUFFER, standing for FACING's FACING-LINE on."
  (align-buffer-pad-add buffer (align-buffer-line-position buffer line) rows
                        'align-buffer-pads-tests-face facing facing-line))

(defun align-buffer-pads-tests--only-pad (buffer)
  "Return the one pad of BUFFER."
  (with-current-buffer buffer
    (car (seq-filter (lambda (overlay)
                       (overlay-get overlay 'align-buffer-pad-rows))
                     (overlays-in (point-min) (point-max))))))


;;; Making a pad

(ert-deftest align-buffer-pads-test-every-row-carries-a-glyph ()
  "A row drawn from nothing can hold no cursor, so each row has a space.
That is what lets the cursor be asked for on a row, which is how a pad is
walked at all.

The opening newline ends the row of the line the pad hangs after and is drawn
in no colour, so that line's own row is left as the reader wrote it.  The last
row has no newline of its own: the line's own newline closes it, which the pad
is drawn over so that it is painted too."
  (align-buffer-pads-tests--with-session "one\ntwo\n"
      "one\na\nb\ntwo\n"
      '((1 . 1) (2 . 4))
    (let ((overlay (align-buffer-pads-tests--pad left 2 2 right 2)))
      (should (equal (overlay-get overlay 'before-string) "\n \n "))
      (should-not (get-text-property 0 'face
                                     (overlay-get overlay 'before-string)))
      (should (eq (get-text-property 1 'face
                                     (overlay-get overlay 'before-string))
                  'align-buffer-pads-tests-face))
      (should (eq (overlay-get overlay 'face)
                  'align-buffer-pads-tests-face)))))

(ert-deftest align-buffer-pads-test-the-opening-keeps-a-coloured-row-painted ()
  "The newline closing the row of the line a pad hangs after keeps its colour.
What paints a row to the edge of the window is the face of the newline that
closes it, and a pad hanging off a coloured line closes that line's row
itself.  Faced with nothing there, the line would be painted only as far as
its text."
  (align-buffer-pads-tests--with-session "one\ntwo\n"
      "one\na\ntwo\n"
      '((1 . 1) (2 . 3))
    (with-current-buffer left
      (overlay-put (make-overlay (align-buffer-line-position left 1)
                                 (align-buffer-line-position left 2))
                   'face 'align-buffer-pads-tests-face))
    (let ((overlay (align-buffer-pads-tests--pad left 2 1 right 2)))
      (should (eq (overlay-get overlay 'align-buffer-pad-opening)
                  'align-buffer-pads-tests-face))
      (should (eq (get-text-property 0 'face
                                     (overlay-get overlay 'before-string))
                  'align-buffer-pads-tests-face)))))

(ert-deftest align-buffer-pads-test-a-pad-above-the-first-line-hangs-on-it ()
  "With no line before the boundary the rows go in front of the first line.
They are then rows of its own display, which is the one place a line cannot
keep its number beside its text, there being nothing above to carry them."
  (align-buffer-pads-tests--with-session "one\ntwo\n"
      "a\nb\none\ntwo\n"
      '((1 . 3) (2 . 4))
    (let ((overlay (align-buffer-pads-tests--pad left 1 2 right 1)))
      (should (= (align-buffer-pad-anchor overlay) (point-min)))
      (should (= (overlay-start overlay) (overlay-end overlay)))
      (should (= (align-buffer-pad-rows overlay) 2))
      ;; No newline to open them and none to close them, so every row carries
      ;; its own.
      (should (equal (overlay-get overlay 'before-string) " \n \n"))
      (should (eq (align-buffer-pad-at (point-min) left) overlay)))))

(ert-deftest align-buffer-pads-test-a-pad-is-found-in-front-of-its-line ()
  "The pad standing before a line is found by that line's own position."
  (align-buffer-pads-tests--with-session "one\ntwo\n"
      "one\na\nb\ntwo\n"
      '((1 . 1) (2 . 4))
    (let ((overlay (align-buffer-pads-tests--pad left 2 2 right 2)))
      (should (eq (align-buffer-pad-at (align-buffer-line-position left 2) left)
                  overlay))
      (should (null (align-buffer-pad-at (align-buffer-line-position left 1)
                                         left)))
      ;; Read in the pane that has not got it, nothing.
      (should (null (align-buffer-pad-at (align-buffer-line-position left 2)
                                         right))))))

(ert-deftest align-buffer-pads-test-clearing-takes-the-rows-away ()
  "A cleared buffer has no pads left and none of them walked."
  (align-buffer-pads-tests--with-session "one\ntwo\n"
      "one\na\nb\ntwo\n"
      '((1 . 1) (2 . 4))
    (align-buffer-pads-tests--pad left 2 2 right 2)
    (with-selected-window (get-buffer-window left)
      (goto-char (align-buffer-line-position left 1))
      (call-interactively #'next-line)
      (should align-buffer--walked)
      (align-buffer-pad-clear left)
      (with-current-buffer left
        (should (null align-buffer--walked))
        (should (null (align-buffer-pads-tests--only-pad left)))))))


(ert-deftest align-buffer-pads-test-rows-above-the-first-line-are-measured ()
  "A pad in front of the first line is counted in a span from the start.
Its rows have no line before them to hang after, so they hang where they are
drawn, and a span beginning there is measured as none of them unless what
hangs at its own beginning is put back."
  (align-buffer-pads-tests--with-session "one\ntwo\nthree\n"
      "a\nb\nc\none\ntwo\nthree\n"
      '((1 . 4) (2 . 5) (3 . 6))
    (let* ((window (get-buffer-window left))
           (height (align-buffer--row-height window))
           (two (align-buffer-line-position left 2))
           (before (align-buffer--offset-in window two))
           (pad (align-buffer-pads-tests--pad left 1 3 right 1)))
      (should (= (align-buffer-pad-hangs pad) (align-buffer-pad-anchor pad)))
      ;; The three rows, drawn above the first line's own text.
      (should (= (align-buffer--offset-in window two) (+ before (* 3 height))))
      (should (= (align-buffer--pad-top window pad) 0)))))


(ert-deftest align-buffer-pads-test-the-row-point-is-on-is-measured-whole ()
  "The row point is on is measured from where that row begins.

A span to a position part way along a row is measured as far as the bottom of
that row, which reads as the row below it: whatever is levelled against the
line the reader is on would be put a row under it."
  (align-buffer-pads-tests--with-session "one
two three
four
"
      "one
two three
four
"
      '((1 . 1) (2 . 2) (3 . 3))
    (let ((window (get-buffer-window left)))
      (with-current-buffer left
        (goto-char (align-buffer-line-position left 2))
        (forward-char 4)
        (set-window-point window (point)))
      ;; The row point is on is the second line's own row, wherever along it
      ;; point sits.
      (should (= (align-buffer--point-row-top window)
                 (align-buffer--offset-in
                  window (align-buffer-line-position left 2))))
      ;; And that is not what measuring to point itself gives.
      (should (< (align-buffer--point-row-top window)
                 (align-buffer--offset-in window (window-point window)))))))

;;; Walking a pad

(ert-deftest align-buffer-pads-test-walking-moves-the-cursor-not-point ()
  "Each step asks for the cursor one row further down and leaves point alone.
Point waits at the end of the line the rows hang after, which is where they
are drawn and so the only place a row of them can carry the cursor."
  (align-buffer-pads-tests--with-session "one\ntwo\n"
      "one\na\nb\nc\ntwo\n"
      '((1 . 1) (2 . 5))
    (align-buffer-pads-tests--pad left 2 3 right 2)
    (with-current-buffer left
      (goto-char (align-buffer-line-position left 1))
      (call-interactively #'next-line)
      (should (equal (cdr align-buffer--walked) 0))
      (should (= (point) (align-buffer-pad-hangs (car align-buffer--walked))))
      (should (= (line-number-at-pos (point)) 1))
      (call-interactively #'next-line)
      (should (equal (cdr align-buffer--walked) 1))
      (should (= (line-number-at-pos (point)) 1))
      ;; The row being walked is the one asking for the cursor, which for the
      ;; second row is the space after the opening newline and the first row's
      ;; own.
      (let ((string (overlay-get (car align-buffer--walked) 'before-string)))
        (should (get-text-property 3 'cursor string))
        (should-not (get-text-property 1 'cursor string))))))

(ert-deftest align-buffer-pads-test-walking-out-reaches-the-line-below ()
  "The step after the last row of a pad leaves it, cursor back on the text."
  (align-buffer-pads-tests--with-session "one\ntwo\n"
      "one\na\nb\ntwo\n"
      '((1 . 1) (2 . 4))
    (let ((overlay (align-buffer-pads-tests--pad left 2 2 right 2)))
      (with-current-buffer left
        (goto-char (align-buffer-line-position left 1))
        (dotimes (_ 3) (call-interactively #'next-line))
        (should (null align-buffer--walked))
        (should (= (line-number-at-pos (point)) 2))
        (should-not (get-text-property 0 'cursor
                                       (overlay-get overlay 'before-string)))))))

(ert-deftest align-buffer-pads-test-walking-up-out-takes-one-press ()
  "The step up from the first row lands on the text of the line it hangs after.
The place point waits while a row is read counts as the rows themselves, so a
reader left standing there would spend the press after this one getting off
them and appear not to have moved at all."
  (align-buffer-pads-tests--with-session "one\ntwo\nthree\n"
      "one\ntwo\na\nb\nthree\n"
      '((1 . 1) (2 . 2) (3 . 5))
    (align-buffer-pads-tests--pad left 3 2 right 3)
    (with-selected-window (get-buffer-window left)
      (goto-char (align-buffer-line-position left 3))
      (dotimes (_ 2) (call-interactively #'previous-line))
      (should (equal (cdr align-buffer--walked) 0))
      (call-interactively #'previous-line)
      (should (null align-buffer--walked))
      (should (= (line-number-at-pos (point)) 2))
      (should-not (= (point) (line-end-position)))
      ;; And the press after it moves a line, rather than being spent.
      (call-interactively #'previous-line)
      (should (= (line-number-at-pos (point)) 1)))))

(ert-deftest align-buffer-pads-test-walking-back-up-returns-to-the-rows ()
  "Stepping up from the line below a pad walks its rows from the bottom."
  (align-buffer-pads-tests--with-session "one\ntwo\n"
      "one\na\nb\nc\ntwo\n"
      '((1 . 1) (2 . 5))
    (align-buffer-pads-tests--pad left 2 3 right 2)
    (with-current-buffer left
      (goto-char (align-buffer-line-position left 2))
      (call-interactively #'previous-line)
      (should (equal (cdr align-buffer--walked) 2))
      (call-interactively #'previous-line)
      (should (equal (cdr align-buffer--walked) 1)))))

(ert-deftest align-buffer-pads-test-moving-point-ends-the-walk ()
  "A command that moves point gives the cursor back to the text."
  (align-buffer-pads-tests--with-session "one\ntwo\n"
      "one\na\nb\ntwo\n"
      '((1 . 1) (2 . 4))
    (align-buffer-pads-tests--pad left 2 2 right 2)
    ;; The pane the walk is in has to be the selected one, since the check
    ;; after a command speaks for the window the reader stands in.
    (with-selected-window (get-buffer-window left)
      (goto-char (align-buffer-line-position left 1))
      (call-interactively #'next-line)
      (should align-buffer--walked)
      (let ((this-command 'forward-char))
        (forward-char 1)
        (align-buffer--walk-check))
      (should (null align-buffer--walked))
      (should (null align-buffer-inhibit-sync)))))

(ert-deftest align-buffer-pads-test-a-walk-is-held-by-where-point-is ()
  "The walk lasts while point is on the line the pad hangs on, and no longer.
So a prefix key or a look at the documentation leaves the reader on their row,
and any command at all that moves point ends the walk, named or not."
  (align-buffer-pads-tests--with-session "one\ntwo\n"
      "one\na\nb\ntwo\n"
      '((1 . 1) (2 . 4))
    (align-buffer-pads-tests--pad left 2 2 right 2)
    (with-current-buffer left
      (goto-char (align-buffer-line-position left 1))
      (call-interactively #'next-line)
      (should (align-buffer--walk-kept-p))
      (forward-char 1)
      (should-not (align-buffer--walk-kept-p)))))


;;; Where rows hang when the line before them is empty

(ert-deftest align-buffer-pads-test-rows-hang-after-an-empty-line ()
  "Rows whose line before is empty hang where they belong in front of instead.

They are drawn in front of the position they hang at, so the end of the line
before is exactly right while that line has text: the rows come after it.  An
empty line's end is its own beginning, and rows hung there are drawn in front
of the line itself - so that line is drawn a pad's height below its pair while
everything above and below it is level."
  (align-buffer-pads-tests--with-session "one\n\nthree\nfour\n"
      "one\n\na\nb\nthree\nfour\n"
      '((1 . 1) (2 . 2) (3 . 5) (4 . 6))
    (let ((overlay (align-buffer-pads-tests--pad left 3 2 right 3)))
      (should (= (overlay-start overlay)
                 (align-buffer-line-position left 3)))
      ;; Which is the shape the rows above the first line of a buffer have,
      ;; and what tells the drawing it needs no newline to open them.
      (should (= (overlay-start overlay) (align-buffer-pad-anchor overlay))))))

(ert-deftest align-buffer-pads-test-rows-hang-after-a-line-with-text ()
  "Rows whose line before has text hang at the end of it, where they come after."
  (align-buffer-pads-tests--with-session "one\ntwo\nthree\nfour\n"
      "one\ntwo\na\nb\nthree\nfour\n"
      '((1 . 1) (2 . 2) (3 . 5) (4 . 6))
    (let ((overlay (align-buffer-pads-tests--pad left 3 2 right 3))
          (ends (with-current-buffer left
                  (save-excursion
                    (goto-char (align-buffer-line-position left 2))
                    (line-end-position)))))
      (should (= (overlay-start overlay) ends))
      (should-not (= (overlay-start overlay)
                     (align-buffer-pad-anchor overlay))))))


;;; A step asked for against a line being measured

(ert-deftest align-buffer-pads-test-measuring-a-line-end-takes-no-step ()
  "`move-end-of-line' steps a line to find where the one it is on ends.
It does that inside `save-excursion' and with the goal column held at nothing,
so it is not a reader going anywhere: a step onto a pad's rows there moves the
cursor, starts a walk of a line nobody asked for, and leaves the line a
linewise operator measures the wrong one."
  (align-buffer-pads-tests--with-session "one\ntwo\nthree\n"
      "one\ntwo\na\nb\nthree\n"
      '((1 . 1) (2 . 2) (3 . 5))
    (align-buffer-pads-tests--pad left 3 2 right 3)
    (with-selected-window (get-buffer-window left)
      (goto-char (align-buffer-line-position left 2))
      (move-end-of-line nil)
      (should (null align-buffer--walked))
      (should (= (point) (line-end-position)))
      (should (= (line-number-at-pos) 2)))))

(ert-deftest align-buffer-pads-test-a-step-of-the-reader-s-own-still-walks ()
  "A vertical motion asks `line-move' for a move, and still reaches the rows."
  (align-buffer-pads-tests--with-session "one\ntwo\nthree\n"
      "one\ntwo\na\nb\nthree\n"
      '((1 . 1) (2 . 2) (3 . 5))
    (align-buffer-pads-tests--pad left 3 2 right 3)
    (with-selected-window (get-buffer-window left)
      (goto-char (align-buffer-line-position left 2))
      (call-interactively #'next-line)
      (should (equal (cdr align-buffer--walked) 0)))))


;;; The cursor against another package's own idea of where it goes

;; A row carries the cursor only while point is where the rows are drawn, and
;; evil moves the cursor one character back off the end of a line after every
;; command it runs.  So the walk has to survive a nudge along that line, and
;; point has to be put back where the rows are before the screen is drawn.

(ert-deftest align-buffer-pads-test-a-nudge-along-the-line-is-taken-back ()
  "Point nudged along the line the rows hang on goes back where they are drawn.
Off that line it is the reader's own, and nothing is taken back: the walk is
over by then and the cursor belongs to the text."
  (align-buffer-pads-tests--with-session "one\ntwo\nthree\n"
      "one\ntwo\na\nb\nthree\n"
      '((1 . 1) (2 . 2) (3 . 5))
    (align-buffer-pads-tests--pad left 3 2 right 3)
    (with-current-buffer left
      (goto-char (align-buffer-line-position left 2))
      (call-interactively #'next-line)
      (let ((hangs (align-buffer-pad-hangs (car align-buffer--walked))))
        (should (= (point) hangs))
        (backward-char 1)
        (align-buffer--walk-hold-point)
        (should (= (point) hangs))
        ;; Another line is the reader having moved, and stays theirs.
        (goto-char (align-buffer-line-position left 1))
        (align-buffer--walk-hold-point)
        (should (= (point) (align-buffer-line-position left 1)))))))

(ert-deftest align-buffer-pads-test-the-reader-moving-along-the-line-ends-it ()
  "A command of the reader's that moves point along that line is not a nudge.
Only the command that stepped is allowed one, so standing on the line a pad
hangs on is standing on the line rather than reading its rows."
  (align-buffer-pads-tests--with-session "one\ntwo\nthree\n"
      "one\ntwo\na\nb\nthree\n"
      '((1 . 1) (2 . 2) (3 . 5))
    (align-buffer-pads-tests--pad left 3 2 right 3)
    (with-selected-window (get-buffer-window left)
      (goto-char (align-buffer-line-position left 2))
      (let ((this-command 'next-line))
        (call-interactively #'next-line)
        (align-buffer--walk-check))
      (should align-buffer--walked)
      ;; A command of their own, a press later, along the same line.
      (let ((this-command 'backward-char))
        (backward-char 1)
        (align-buffer--walk-check))
      (should (null align-buffer--walked))
      (should-not (= (point) (align-buffer-line-position left 2))))))

(ert-deftest align-buffer-pads-test-a-pullback-leaves-the-row-its-cursor ()
  "A package pulling the cursor back after the command has point put back.
Whoever moved it ran in the same hook, so what decides is that we run after
them: the row is drawn from where point is, and nothing else gets a say once
the command is over."
  (align-buffer-pads-tests--with-session "one\ntwo\nthree\n"
      "one\ntwo\na\nb\nthree\n"
      '((1 . 1) (2 . 2) (3 . 5))
    (align-buffer-pads-tests--pad left 3 2 right 3)
    (with-selected-window (get-buffer-window left)
      (goto-char (align-buffer-line-position left 2))
      ;; Added the way evil adds its own, which is in front of ours.
      (add-hook 'post-command-hook #'backward-char nil t)
      (unwind-protect
          (let ((this-command 'next-line))
            (call-interactively #'next-line)
            (run-hooks 'post-command-hook)
            (should align-buffer--walked)
            (should (= (point)
                       (align-buffer-pad-hangs (car align-buffer--walked)))))
        (remove-hook 'post-command-hook #'backward-char t)))))


;;; Scrolling through the rows

(ert-deftest align-buffer-pads-test-a-scroll-that-moves-nothing-says-so ()
  "A scroll with nowhere to go signals, as the commands we stand in for do.
`evil-scroll-down' and the rest have the end of their own work to do when the
view cannot move, and a reader is told they are at the end of the buffer."
  (align-buffer-pads-tests--with-session "one\ntwo\n"
      "one\na\ntwo\n"
      '((1 . 1) (2 . 3))
    (align-buffer-pads-tests--pad left 2 1 right 2)
    (with-selected-window (get-buffer-window left)
      (goto-char (point-min))
      (should-error (scroll-down 1) :type 'beginning-of-buffer))))

(ert-deftest align-buffer-pads-test-a-scroll-gives-the-windows-back ()
  "A scroll out of a run hands the pairs the two panes again.
Both panes are ours while a run of lines the other side has not got is read,
and a scroll that leaves the run has to say so or the pairs never speak for
them again."
  (let* ((lines (lambda (from to)
                  (mapconcat (lambda (number) (format "line %03d\n" number))
                             (number-sequence from to) "")))
         (tail (funcall lines 2 80)))
    (align-buffer-pads-tests--with-session
        (concat "line 001\n" tail)
        (concat "line 001\nadded one\nadded two\n" tail)
        (cons '(1 . 1)
              (mapcar (lambda (number) (cons number (+ number 2)))
                      (number-sequence 2 80)))
      ;; The rows the old side reads in place of the two lines it has not got.
      (align-buffer-pads-tests--pad left 2 2 right 2)
      (with-selected-window (get-buffer-window right)
        ;; On a line of the run the old side has not got, so the pad standing
        ;; for it over there is read and both panes are held.
        (goto-char (align-buffer-line-position right 3))
        (let ((this-command 'next-line)) (align-buffer--walk-check))
        (should align-buffer-inhibit-sync)
        (scroll-up 20)
        (let ((this-command 'scroll-up-command)) (align-buffer--walk-check))
        (should-not align-buffer-inhibit-sync)))))

;;; What a step reports

;; `line-move' answers whether it moved, and the commands built on it read that
;; answer: `move-end-of-line' steps a line, is told it moved, and comes back a
;; character to the newline it passed.  Told otherwise it stops at the
;; beginning of the next line, and everything measuring a line from there -
;; evil's `yy', `dd' and `cc' among them - takes a line too many.

(ert-deftest align-buffer-pads-test-a-step-that-moved-says-so ()
  "A step of a line answers the way `line-move' does, having moved."
  (align-buffer-pads-tests--with-session "one\ntwo\nthree\n"
      "one\ntwo\nthree\n"
      '((1 . 1) (2 . 2) (3 . 3))
    (with-selected-window (get-buffer-window left)
      (goto-char (align-buffer-line-position left 1))
      (should (line-move 1 t))
      (should (= (line-number-at-pos (point)) 2)))))

(ert-deftest align-buffer-pads-test-a-step-with-nowhere-to-go-says-so ()
  "A step off the end of the buffer answers that it could not move."
  (align-buffer-pads-tests--with-session "one\ntwo\n"
      "one\ntwo\n"
      '((1 . 1) (2 . 2))
    (with-selected-window (get-buffer-window left)
      (goto-char (point-max))
      (should-not (line-move 1 t)))))

(ert-deftest align-buffer-pads-test-a-step-into-a-pad-says-it-moved ()
  "A step onto the rows of a pad is a step taken, and answers so."
  (align-buffer-pads-tests--with-session "one\ntwo\n"
      "one\na\nb\ntwo\n"
      '((1 . 1) (2 . 4))
    (align-buffer-pads-tests--pad left 2 2 right 2)
    (with-selected-window (get-buffer-window left)
      (goto-char (align-buffer-line-position left 1))
      (should (line-move 1 t))
      (should (equal (cdr align-buffer--walked) 0))
      ;; And again, onto the second row of the same pad.
      (should (line-move 1 t))
      (should (equal (cdr align-buffer--walked) 1)))))

(ert-deftest align-buffer-pads-test-the-end-of-a-line-is-reached-in-a-pane ()
  "`move-end-of-line' lands on the end of the line point is on.
It steps a line and comes back over the newline, so a pane that tells it the
step failed leaves it at the beginning of the line below, and a command
measuring a line from there takes in the line after it as well."
  (align-buffer-pads-tests--with-session "one\ntwo\nthree\n"
      "one\ntwo\nthree\n"
      '((1 . 1) (2 . 2) (3 . 3))
    (with-selected-window (get-buffer-window left)
      (goto-char (align-buffer-line-position left 2))
      (move-end-of-line nil)
      (should (= (point) (line-end-position)))
      (should (= (line-number-at-pos (point)) 2)))))

;;; Answering a window that changes size

(defun align-buffer-pads-tests--facing-rows (facing facing-line lines)
  "Return how many rows LINES lines of FACING from FACING-LINE are drawn as."
  (length (car (align-buffer--pad-run-rows facing facing-line lines))))

(defmacro align-buffer-pads-tests--with-wrapping-session (&rest body)
  "Run BODY over a session whose right side has two lines long enough to wrap.
The left stands a pad of two rows in front of its own second line for them."
  (declare (indent 0))
  `(align-buffer-pads-tests--with-session
       "one\ntwo\n"
       ,(concat "one\n" (make-string 100 ?x) "\n" (make-string 100 ?y)
                "\ntwo\n")
       '((1 . 1) (2 . 4))
     (let ((pad (align-buffer-pads-tests--pad left 2 2 right 2)))
       ,@body)))

(ert-deftest align-buffer-pads-test-a-width-change-is-measured-off-redisplay ()
  "A width change queues the measure instead of taking it there and then.
`window-size-change-functions' runs from redisplay, where a window we place is
put back by the pass that follows."
  (align-buffer-pads-tests--with-wrapping-session
    (adjust-window-trailing-edge (get-buffer-window left) -20 t)
    (align-buffer--size-changed (selected-frame))
    (should align-buffer--resize-timer)
    (should (equal (align-buffer-pad-rows pad) 2))
    (align-buffer--measure-and-level)
    (should-not align-buffer--resize-timer)))

(ert-deftest align-buffer-pads-test-a-pad-is-measured-again-at-a-new-width ()
  "A pad stands as many rows as the facing run is drawn as at the width it has.
The run wraps differently once the facing window changes width, and the pad is
measured against it again."
  (align-buffer-pads-tests--with-wrapping-session
    (should (equal (align-buffer-pad-rows pad)
                   (align-buffer-pads-tests--facing-rows right 2 2)))
    (adjust-window-trailing-edge (get-buffer-window left) -20 t)
    (align-buffer--measure-and-level)
    (should (equal (align-buffer-pad-rows pad)
                   (align-buffer-pads-tests--facing-rows right 2 2)))
    ;; And back to a width it has already been measured at.
    (adjust-window-trailing-edge (get-buffer-window left) 20 t)
    (align-buffer--measure-and-level)
    (should (equal (align-buffer-pad-rows pad)
                   (align-buffer-pads-tests--facing-rows right 2 2)))))

(ert-deftest align-buffer-pads-test-a-measured-pad-has-a-row-for-each-line ()
  "Each row of a pad measured again says which line of the run it stands for,
and each line of the run the row its own text faces."
  (align-buffer-pads-tests--with-wrapping-session
    (adjust-window-trailing-edge (get-buffer-window left) -20 t)
    (align-buffer--measure-and-level)
    (let ((rows (car (overlay-get pad 'align-buffer-pad-run))))
      (should (equal (length rows) (align-buffer-pad-rows pad)))
      (should (equal (aref rows 0) 2))
      (should (equal (aref rows (1- (length rows))) 3))
      (should (equal (align-buffer-pad-text-row pad 2) 0))
      (should (equal (align-buffer-pad-lines pad) 2)))))

(ert-deftest align-buffer-pads-test-a-measured-pad-keeps-the-row-walked ()
  "The row a pad is being walked at is still the cursor's once it is measured
again, and the last row where the run has lost the one it was on."
  (align-buffer-pads-tests--with-wrapping-session
    (with-selected-window (get-buffer-window left)
      (goto-char (align-buffer-line-position left 1))
      (line-move 1 t)
      (line-move 1 t)
      (should (equal (cdr align-buffer--walked) 1))
      (adjust-window-trailing-edge (get-buffer-window left) -20 t)
      (align-buffer--measure-and-level)
      (should (equal (car align-buffer--walked) pad))
      (should (equal (cdr align-buffer--walked) 1))
      (should (< (cdr align-buffer--walked) (align-buffer-pad-rows pad))))))

(ert-deftest align-buffer-pads-test-a-width-change-is-answered-without-a-pad ()
  "A session with no pads in it answers a width change as well.
Lines long enough to wrap are drawn as another number of rows at another
width, which moves every line below the first of them."
  (align-buffer-pads-tests--with-session
      "one\ntwo\n"
      (concat "one\n" (make-string 100 ?x) "\ntwo\n")
      '((1 . 1) (2 . 3))
    (adjust-window-trailing-edge (get-buffer-window left) -20 t)
    (align-buffer--size-changed (selected-frame))
    (should align-buffer--resize-timer)
    (align-buffer--measure-and-level)
    (should-not align-buffer--resize-timer)))

(ert-deftest align-buffer-pads-test-a-pad-with-no-facing-window-stands ()
  "A pad whose facing side is shown nowhere keeps the rows it was given.
Nothing says how wide those lines wrap, so a measure of none is no answer."
  (align-buffer-pads-tests--with-wrapping-session
    (adjust-window-trailing-edge (get-buffer-window left) -20 t)
    (delete-window (get-buffer-window right))
    (align-buffer--measure-and-level)
    (should (equal (align-buffer-pad-rows pad) 2))))

(provide 'align-buffer-pads-tests)
;;; align-buffer-pads-tests.el ends here
