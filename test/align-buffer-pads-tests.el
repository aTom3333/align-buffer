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

(provide 'align-buffer-pads-tests)
;;; align-buffer-pads-tests.el ends here
