;;; align-buffer-sections-tests.el --- Tests for moving between sections -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   emacs -Q --batch -L . -l test/align-buffer-sections-tests.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; The sections are named by hand here, the way a consumer names them, so what
;; is tested is the move rather than whatever worked out that a stretch of the
;; two buffers was worth stopping at.

;;; Code:

(require 'ert)
(require 'align-buffer)

(defface align-buffer-sections-tests-face '((t :extend t))
  "A face standing in for whatever colour a consumer draws its rows in.")

(defmacro align-buffer-sections-tests--with-session (left-text right-text pairs
                                                               sections
                                                               &rest body)
  "Show LEFT-TEXT against RIGHT-TEXT, related by PAIRS, and run BODY.
SECTIONS are the stretches to move between.  `left', `right' and `session'
are bound, and both buffers walk their pads."
  (declare (indent 4))
  `(let ((left (generate-new-buffer " *align-buffer sections left*"))
         (right (generate-new-buffer " *align-buffer sections right*")))
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
                        (align-buffer-set-sections session ,sections)
                        ,@body)
               (align-buffer-quit session))))
       (kill-buffer left)
       (kill-buffer right))))

(defun align-buffer-sections-tests--lines (from to)
  "Return the text of the lines FROM to TO, one numbered line each."
  (mapconcat (lambda (number) (format "line %03d\n" number))
             (number-sequence from to) ""))

(defun align-buffer-sections-tests--pad (buffer line rows &optional facing
                                                facing-line)
  "Hang ROWS rows above LINE of BUFFER, standing for FACING's FACING-LINE on."
  (align-buffer-pad-add buffer (align-buffer-line-position buffer line) rows
                        'align-buffer-sections-tests-face facing facing-line))

;; Six lines against eight: the right has two the left has not got in front of
;; left line 3, and both have two lines of their own at left 5.  Those are the
;; two sections, and the one the left has no lines of is the only one either
;; side is missing.
(defconst align-buffer-sections-tests--left
  (align-buffer-sections-tests--lines 1 6)
  "The left text of the two-section session.")

(defconst align-buffer-sections-tests--right
  (concat (align-buffer-sections-tests--lines 1 2)
          "added one\nadded two\n"
          (align-buffer-sections-tests--lines 3 6))
  "The right text of the two-section session.")

(defconst align-buffer-sections-tests--pairs
  '((1 . 1) (2 . 2) (3 . 5) (4 . 6) (5 . 7) (6 . 8))
  "What corresponds in the two-section session.")

(defconst align-buffer-sections-tests--sections '((3 0 3 2) (5 2 7 2))
  "The sections of the two-section session.")

(defmacro align-buffer-sections-tests--with-two-sections (&rest body)
  "Show the two-section session and run BODY, with a pad on the left."
  (declare (indent 0))
  `(align-buffer-sections-tests--with-session
       align-buffer-sections-tests--left
       align-buffer-sections-tests--right
       align-buffer-sections-tests--pairs
       align-buffer-sections-tests--sections
     (align-buffer-sections-tests--pad left 3 2 right 3)
     ,@body))


;;; What a consumer marks

(ert-deftest align-buffer-sections-test-a-section-is-a-run-on-each-side ()
  "A section says where it begins and how many lines it has on each side."
  (align-buffer-sections-tests--with-two-sections
    (should (equal (align-buffer-session-sections session)
                   '((3 0 3 2) (5 2 7 2))))
    (should (= (align-buffer-section-line '(3 0 7 2) 'left) 3))
    (should (= (align-buffer-section-count '(3 0 7 2) 'left) 0))
    (should (= (align-buffer-section-line '(3 0 7 2) 'right) 7))
    (should (= (align-buffer-section-count '(3 0 7 2) 'right) 2))))

(ert-deftest align-buffer-sections-test-sections-may-be-given-as-a-vector ()
  "A vector of sections is kept as a list, as the pairs are."
  (align-buffer-sections-tests--with-two-sections
    (align-buffer-set-sections session [(1 1 1 1)])
    (should (equal (align-buffer-session-sections session) '((1 1 1 1))))))

(ert-deftest align-buffer-sections-test-anything-else-is-refused ()
  "A section that is not a run of lines on each side signals when it is set."
  (should-error (align-buffer-check-sections '((1 1 1))))
  (should-error (align-buffer-check-sections '((1 . 1))))
  (should-error (align-buffer-check-sections '((0 1 1 1))))
  (should-error (align-buffer-check-sections '((1 -1 1 1))))
  (should-error (align-buffer-check-sections '(("1" 1 1 1))))
  (align-buffer-check-sections '((1 0 1 3) (9 2 11 2))))


;;; Moving from one to the next

(ert-deftest align-buffer-sections-test-the-next-section-is-moved-to ()
  "The move lands on the first line the section has on the reader's side."
  (align-buffer-sections-tests--with-two-sections
    (with-selected-window (get-buffer-window right)
      (goto-char (point-min))
      (call-interactively #'align-buffer-next-section)
      (should (= (line-number-at-pos (point)) 3))
      (call-interactively #'align-buffer-next-section)
      (should (= (line-number-at-pos (point)) 7)))))

(ert-deftest align-buffer-sections-test-each-side-stops-where-its-lines-are ()
  "One section, two places: each pane stops where its own lines of it are.
The right has the two lines at its line 3 and the left has none of them, so
the left stops at the boundary they belong in front of."
  (align-buffer-sections-tests--with-two-sections
    (with-selected-window (get-buffer-window left)
      (goto-char (point-min))
      (align-buffer-next-section)
      (should (equal (cdr align-buffer--walked) 0))
      (should (= (align-buffer-line-at
                  left (align-buffer-pad-anchor (car align-buffer--walked)))
                 3)))))

(ert-deftest align-buffer-sections-test-a-pad-is-walked-into ()
  "Where the side has no lines of a section, its pad's first row is read.
Point waits at the end of the line the rows hang after, which is where they
are drawn, and the cursor is asked for on the first of them."
  (align-buffer-sections-tests--with-two-sections
    (with-selected-window (get-buffer-window left)
      (goto-char (point-min))
      (align-buffer-next-section)
      (should (equal (cdr align-buffer--walked) 0))
      (should (= (point) (align-buffer-pad-hangs (car align-buffer--walked))))
      (should (= (line-number-at-pos (point)) 2))
      (should (get-text-property
               1 'cursor
               (overlay-get (car align-buffer--walked) 'before-string))))))

(ert-deftest align-buffer-sections-test-without-the-walk-the-line-below-it-is ()
  "The rows are read only where the mode that walks them is on.
Without it they are display nothing steps through, so the reader goes to the
line the missing ones belong in front of."
  (align-buffer-sections-tests--with-two-sections
    (dolist (buffer (list left right))
      (with-current-buffer buffer (align-buffer-navigation-mode -1)))
    (with-selected-window (get-buffer-window left)
      (goto-char (point-min))
      (align-buffer-next-section)
      (should (null align-buffer--walked))
      (should (= (line-number-at-pos (point)) 3)))))

(ert-deftest align-buffer-sections-test-a-section-with-no-pad-is-reached ()
  "A side with no lines of a section and nothing standing for them still stops.
The boundary is between two lines, so the reader lands on the line below it."
  (align-buffer-sections-tests--with-session
      align-buffer-sections-tests--left
      align-buffer-sections-tests--right
      align-buffer-sections-tests--pairs
      align-buffer-sections-tests--sections
    (with-selected-window (get-buffer-window left)
      (goto-char (point-min))
      (align-buffer-next-section)
      (should (null align-buffer--walked))
      (should (= (line-number-at-pos (point)) 3)))))

(ert-deftest align-buffer-sections-test-a-section-past-the-last-line-is-reached ()
  "A run the other side alone has at the very end belongs past the last line.
There is no line there to go to, so what stands for it hangs at the end of the
text and that is what the move finds."
  (align-buffer-sections-tests--with-session
      (align-buffer-sections-tests--lines 1 2)
      (align-buffer-sections-tests--lines 1 4)
      '((1 . 1) (2 . 2))
      '((3 0 3 2))
    (align-buffer-pad-add left (with-current-buffer left (point-max)) 2
                          'align-buffer-sections-tests-face right 3)
    (with-selected-window (get-buffer-window left)
      (goto-char (point-min))
      (align-buffer-next-section)
      (should (equal (cdr align-buffer--walked) 0))
      (should (= (align-buffer-pad-anchor (car align-buffer--walked))
                 (with-current-buffer left (point-max)))))))

(ert-deftest align-buffer-sections-test-the-reader-inside-one-moves-past-it ()
  "A reader standing in a section is taken out of it, not back to its start."
  (align-buffer-sections-tests--with-two-sections
    (with-selected-window (get-buffer-window right)
      ;; The second line of the section at right 7, whose first line is 7.
      (goto-char (align-buffer-line-position right 8))
      (align-buffer-previous-section)
      (should (= (line-number-at-pos (point)) 3))
      (goto-char (align-buffer-line-position right 8))
      (should-error (align-buffer-next-section) :type 'user-error))))

(ert-deftest align-buffer-sections-test-a-walked-pad-is-the-section-stood-in ()
  "A reader reading a pad stands in the section it is the picture of.
So the move out of it goes to the section after, and the one back goes to the
section before rather than to the rows they are already on."
  (align-buffer-sections-tests--with-two-sections
    (with-selected-window (get-buffer-window left)
      (goto-char (point-min))
      (align-buffer-next-section)
      (should (equal (cdr align-buffer--walked) 0))
      (align-buffer-next-section)
      (should (null align-buffer--walked))
      (should (= (line-number-at-pos (point)) 5)))))

(ert-deftest align-buffer-sections-test-a-count-moves-that-many ()
  "A numeric argument says how many sections to move by."
  (align-buffer-sections-tests--with-two-sections
    ;; A third section, at the first line of both, to have two to count back
    ;; over as well as forwards.
    (align-buffer-set-sections session '((1 1 1 1) (3 0 3 2) (5 2 7 2)))
    (with-selected-window (get-buffer-window right)
      (goto-char (point-min))
      (align-buffer-next-section 2)
      (should (= (line-number-at-pos (point)) 7))
      (align-buffer-previous-section 2)
      (should (= (line-number-at-pos (point)) 1)))))

(ert-deftest align-buffer-sections-test-the-section-is-left-reading-centred ()
  "The window is scrolled so the section's lines read in the middle of it.
A pad counts the line its rows hang after, whose text is drawn on the row
above the first of them."
  (align-buffer-sections-tests--with-session
      (align-buffer-sections-tests--lines 1 60)
      (concat (align-buffer-sections-tests--lines 1 20)
              "added one\nadded two\nadded three\nadded four\n"
              (align-buffer-sections-tests--lines 21 60))
      (append (mapcar (lambda (number) (cons number number))
                      (number-sequence 1 20))
              (mapcar (lambda (number) (cons number (+ number 4)))
                      (number-sequence 21 60)))
      '((21 0 21 4) (40 2 44 2))
    (align-buffer-sections-tests--pad left 21 4 right 21)
    (with-selected-window (get-buffer-window left)
      (let ((height (window-body-height)))
        (goto-char (point-min))
        (align-buffer-next-section)
        ;; Five rows centred: the four of the pad and the line they hang
        ;; after, which is the line point is left on.
        (should (= (align-buffer-line-at left (window-start))
                   (- 20 (/ (- height 5) 2))))
        (align-buffer-next-section)
        (should (= (align-buffer-line-at left (window-start))
                   (- 40 (/ (- height 2) 2))))))))


;;; Running out of sections

(defmacro align-buffer-sections-tests--with-handler (handled &rest body)
  "Run BODY with a boundary handler returning HANDLED, and `calls' bound.
`calls' collects the (SESSION . REMAINING) the handler was given, in order."
  (declare (indent 1))
  `(let* ((calls nil)
          (align-buffer-section-boundary-functions
           (list (lambda (boundary-session remaining)
                   (setq calls (append calls (list (cons boundary-session
                                                         remaining))))
                   ,handled))))
     ,@body))

(ert-deftest align-buffer-sections-test-a-handler-stands-in-for-the-error ()
  "A handler taking the boundary leaves the command quiet and the reader put."
  (align-buffer-sections-tests--with-two-sections
    (align-buffer-sections-tests--with-handler t
      (with-selected-window (get-buffer-window right)
        (goto-char (align-buffer-line-position right 7))
        (align-buffer-next-section)
        (should (= (line-number-at-pos (point)) 7))
        (should (equal calls (list (cons session 1))))))))

(ert-deftest align-buffer-sections-test-a-handler-may-decline ()
  "A handler returning nil has not taken the boundary, so the error stands."
  (align-buffer-sections-tests--with-two-sections
    (align-buffer-sections-tests--with-handler nil
      (with-selected-window (get-buffer-window right)
        (goto-char (align-buffer-line-position right 7))
        (should-error (align-buffer-next-section) :type 'user-error)
        (should (equal calls (list (cons session 1))))))))

(ert-deftest align-buffer-sections-test-a-handled-boundary-ends-the-move ()
  "The moves left over are not spent once a handler has taken the boundary.
It may have moved the reader somewhere else, where they mean nothing."
  (align-buffer-sections-tests--with-two-sections
    (align-buffer-sections-tests--with-handler t
      (with-selected-window (get-buffer-window right)
        (goto-char (point-min))
        (align-buffer-next-section 4)
        (should (= (line-number-at-pos (point)) 7))
        (should (equal calls (list (cons session 2))))))))

(ert-deftest align-buffer-sections-test-a-backward-boundary-counts-down ()
  "Going backwards the moves left are negative, so a handler knows which way."
  (align-buffer-sections-tests--with-two-sections
    (align-buffer-sections-tests--with-handler t
      (with-selected-window (get-buffer-window right)
        (goto-char (point-min))
        (align-buffer-previous-section)
        (should (equal calls (list (cons session -1))))))))

(ert-deftest align-buffer-sections-test-no-sections-at-all-reaches-the-hook ()
  "A session nobody has marked anything in is a boundary like running out."
  (align-buffer-sections-tests--with-session
      align-buffer-sections-tests--left
      align-buffer-sections-tests--right
      align-buffer-sections-tests--pairs
      nil
    (align-buffer-sections-tests--with-handler t
      (with-selected-window (get-buffer-window right)
        (goto-char (point-min))
        (align-buffer-next-section)
        (should (equal calls (list (cons session 1))))))))

(ert-deftest align-buffer-sections-test-a-handler-for-one-session-only ()
  "The hook made buffer-local in a pane is that session's own handler."
  (align-buffer-sections-tests--with-two-sections
    (let ((taken nil))
      (with-selected-window (get-buffer-window right)
        (add-hook 'align-buffer-section-boundary-functions
                  (lambda (_session _remaining) (setq taken t))
                  nil t)
        (goto-char (align-buffer-line-position right 7))
        (align-buffer-next-section)
        (should taken)))))

(ert-deftest align-buffer-sections-test-a-buffer-in-no-session-says-so ()
  "The commands are for a pane, and anywhere else is an error of the reader's."
  (with-temp-buffer
    (should-error (align-buffer-next-section) :type 'user-error)))

(provide 'align-buffer-sections-tests)
;;; align-buffer-sections-tests.el ends here
