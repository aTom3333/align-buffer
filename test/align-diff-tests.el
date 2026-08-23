;;; align-diff-tests.el --- Tests for align-diff -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   emacs -Q --batch -L . -L extensions -l test/align-diff-tests.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'align-diff)

(defmacro align-diff-tests--with-buffers (old-text new-text &rest body)
  "Run BODY with `old' and `new' bound to buffers holding OLD-TEXT and NEW-TEXT."
  (declare (indent 2))
  `(let ((old (generate-new-buffer " *align-diff test old*"))
         (new (generate-new-buffer " *align-diff test new*")))
     (unwind-protect
         (progn
           (with-current-buffer old (insert ,old-text))
           (with-current-buffer new (insert ,new-text))
           ,@body)
       (kill-buffer old)
       (kill-buffer new))))

(defun align-diff-tests--shape (plan)
  "Return PLAN's rows as (TAG LEFT-LINE RIGHT-LINE) triples.
A blank cell reads as nil, so the shape says which side a row stands on."
  (mapcar (lambda (row)
            (list (align-buffer-row-tag row)
                  (align-buffer-cell-line (align-buffer-row-left row))
                  (align-buffer-cell-line (align-buffer-row-right row))))
          (append (align-buffer-plan-rows plan) nil)))

(defun align-diff-tests--faces (plan)
  "Return PLAN's rows as (LEFT-FACE . RIGHT-FACE) pairs."
  (mapcar (lambda (row)
            (cons (align-buffer-cell-face (align-buffer-row-left row))
                  (align-buffer-cell-face (align-buffer-row-right row))))
          (append (align-buffer-plan-rows plan) nil)))

(ert-deftest align-diff-test-identical-buffers-refuse ()
  "Two buffers holding the same text have nothing to show."
  (align-diff-tests--with-buffers "one\ntwo\n" "one\ntwo\n"
    (should-error (align-diff-plan old new) :type 'user-error)))

(ert-deftest align-diff-test-context-rows-read-both-sides ()
  "An unchanged line is one row reading its own line number on each side."
  (align-diff-tests--with-buffers "one\ntwo\n" "one\nTWO\n"
    (should (equal (align-diff-tests--shape (align-diff-plan old new))
                   '((nil 1 1) (change 2 2))))))

(ert-deftest align-diff-test-a-changed-line-pairs-into-one-row ()
  "A line edited in place is one row, faced as removed on the left and added on
the right."
  (align-diff-tests--with-buffers "one\ntwo\n" "one\nTWO\n"
    (should (equal (align-diff-tests--faces (align-diff-plan old new))
                   '((nil . nil)
                     (align-diff-removed . align-diff-added))))))

(ert-deftest align-diff-test-an-addition-pads-the-old-side ()
  "A line only the new side has puts padding on the left, faced as padding."
  (align-diff-tests--with-buffers "one\ntwo\n" "one\nextra\ntwo\n"
    (let ((plan (align-diff-plan old new)))
      (should (equal (align-diff-tests--shape plan)
                     '((nil 1 1) (change nil 2) (nil 2 3))))
      (should (equal (nth 1 (align-diff-tests--faces plan))
                     '(align-diff-padding-old . align-diff-added))))))

(ert-deftest align-diff-test-a-removal-pads-the-new-side ()
  "A line only the old side has puts padding on the right."
  (align-diff-tests--with-buffers "one\ngone\ntwo\n" "one\ntwo\n"
    (let ((plan (align-diff-plan old new)))
      (should (equal (align-diff-tests--shape plan)
                     '((nil 1 1) (change 2 nil) (nil 3 2))))
      (should (equal (nth 1 (align-diff-tests--faces plan))
                     '(align-diff-removed . align-diff-padding-new))))))

(ert-deftest align-diff-test-an-uneven-run-pairs-then-stands-alone ()
  "Three lines replaced by one: one changed row, then two removals."
  (align-diff-tests--with-buffers "a\nb\nc\nd\ne\n" "a\nB\ne\n"
    (should (equal (align-diff-tests--shape (align-diff-plan old new))
                   '((nil 1 1) (change 2 2) (change 3 nil) (change 4 nil)
                     (nil 5 3))))))

(ert-deftest align-diff-test-a-run-of-differences-is-one-section ()
  "Adjacent differing rows share a tag, so section navigation moves hunk to hunk."
  (align-diff-tests--with-buffers "a\nb\nc\nd\ne\nf\n" "a\nB\nC\nd\ne\nF\n"
    (let* ((plan (align-diff-plan old new))
           (rows (vconcat (align-buffer-plan-rows plan))))
      ;; Rows 1 and 2 changed together, row 5 on its own: two hunks.
      (should (equal (align-buffer-derive-sections rows) [(1 . 2) (5 . 5)])))))

(ert-deftest align-diff-test-a-file-without-a-final-newline ()
  "A last line with no newline is still a line of its own."
  (align-diff-tests--with-buffers "one\ntwo" "one\nTWO"
    (should (equal (align-diff-tests--shape (align-diff-plan old new))
                   '((nil 1 1) (change 2 2))))))

(ert-deftest align-diff-test-line-endings-alone-are-not-a-difference ()
  "The default switches ignore a trailing carriage return.
Reviewing a CRLF file against a LF revision otherwise shows every line as
changed."
  (align-diff-tests--with-buffers "one\r\ntwo\r\n" "one\ntwo\n"
    (should-error (align-diff-plan old new) :type 'user-error))
  (align-diff-tests--with-buffers "one\r\ntwo\r\n" "one\ntwo\n"
    (let ((align-diff-switches nil))
      (should (equal (align-diff-tests--shape (align-diff-plan old new))
                     '((change 1 1) (change 2 2)))))))

(defun align-diff-tests--refinements (session side)
  "Return the text of SESSION's refinement overlays in SIDE's pane, in order."
  (with-current-buffer (align-buffer-buffer session side)
    (mapcar (lambda (overlay)
              (buffer-substring-no-properties (overlay-start overlay)
                                              (overlay-end overlay)))
            (sort (seq-filter (lambda (overlay)
                                (overlay-get overlay 'align-diff-refinement))
                              (overlays-in (point-min) (point-max)))
                  (lambda (one other)
                    (< (overlay-start one) (overlay-start other)))))))

(defmacro align-diff-tests--with-session (old-text new-text &rest body)
  "Show OLD-TEXT against NEW-TEXT and run BODY with `session' bound."
  (declare (indent 2))
  `(align-diff-tests--with-buffers ,old-text ,new-text
     (let ((session (align-diff-buffers old new)))
       (unwind-protect (progn ,@body)
         (align-buffer-quit session)))))

(ert-deftest align-diff-test-refinement-marks-what-changed ()
  "A changed line has its differing words marked on each side."
  (align-diff-tests--with-session "int foo(int bar);\n" "int foo(int baz);\n"
    (should (equal (align-diff-tests--refinements session 'left) '("bar")))
    (should (equal (align-diff-tests--refinements session 'right) '("baz")))))

(ert-deftest align-diff-test-refinement-follows-a-line-split ()
  "A hunk is refined whole, so text moved to a new line still corresponds.
Refining line by line would call the second argument removed on one row and
added on another, when all that happened is a line break."
  (align-diff-tests--with-session
      "one\ncall(first, second);\nthree\n"
      "one\ncall(first,\n     second_renamed);\nthree\n"
    (should (equal (align-diff-tests--refinements session 'left) nil))
    (should (equal (align-diff-tests--refinements session 'right) '("_renamed")))))

(ert-deftest align-diff-test-a-one-sided-hunk-is-not-refined ()
  "A hunk that is all insertion has nothing to correspond with."
  (align-diff-tests--with-session "one\ntwo\n" "one\nextra\ntwo\n"
    (should (null (align-diff-tests--refinements session 'left)))
    (should (null (align-diff-tests--refinements session 'right)))))

(ert-deftest align-diff-test-refinement-can-be-turned-off ()
  "With `align-diff-refine-hunks' nil the panes carry no refinement."
  (let ((align-diff-refine-hunks nil))
    (align-diff-tests--with-session "int foo(int bar);\n" "int foo(int baz);\n"
      (should (null (align-diff-tests--refinements session 'left)))
      (should (null (align-diff-tests--refinements session 'right))))))

(ert-deftest align-diff-test-refinement-comes-back-after-a-rebuild ()
  "Rendering erases the panes, so the refinement is painted again with them."
  (align-diff-tests--with-session "int foo(int bar);\n" "int foo(int baz);\n"
    (align-buffer-rebuild session (align-buffer-session-plan session))
    (should (equal (align-diff-tests--refinements session 'left) '("bar")))
    (should (equal (align-diff-tests--refinements session 'right) '("baz")))))

(defmacro align-diff-tests--with-files (old-text new-text &rest body)
  "Write OLD-TEXT and NEW-TEXT to temporary files bound to `old' and `new'."
  (declare (indent 2))
  `(let ((old (make-temp-file "align-diff-old-" nil ".txt" ,old-text))
         (new (make-temp-file "align-diff-new-" nil ".txt" ,new-text)))
     (unwind-protect (progn ,@body)
       (dolist (file (list old new))
         (when-let ((buffer (find-buffer-visiting file))) (kill-buffer buffer))
         (delete-file file)))))

(ert-deftest align-diff-test-two-files ()
  "Files are read into buffers, and the ones opened for it are killed after."
  (align-diff-tests--with-files "one
two
" "one
TWO
"
    (let ((session (align-diff-files old new)))
      (should (= (align-buffer-row-count session) 2))
      (should (find-buffer-visiting old))
      (align-buffer-quit session)
      (should-not (find-buffer-visiting old))
      (should-not (find-buffer-visiting new)))))

(ert-deftest align-diff-test-a-file-already-open-is-left-open ()
  "A buffer the reader already had is not killed with the session."
  (align-diff-tests--with-files "one
two
" "one
TWO
"
    (let ((theirs (find-file-noselect old)))
      (let ((session (align-diff-files old new)))
        (align-buffer-quit session)
        (should (buffer-live-p theirs))
        (should-not (find-buffer-visiting new))))))

(ert-deftest align-diff-test-an-edited-source-is-not-killed ()
  "A source the reader has edited survives the teardown, owned or not."
  (align-diff-tests--with-files "one
two
" "one
TWO
"
    (let* ((session (align-diff-files old new))
           (buffer (find-buffer-visiting new)))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert "three
")))
      (align-buffer-quit session)
      (should (buffer-live-p buffer))
      (with-current-buffer buffer (set-buffer-modified-p nil)))))

(ert-deftest align-diff-test-properties-reach-the-plan ()
  "What a caller passes is carried on the plan, beside align-diff's own mark."
  (align-diff-tests--with-buffers "one
two
" "one
TWO
"
    (let* ((extra (generate-new-buffer " *align-diff owned*"))
           (properties (align-buffer-plan-properties
                        (align-diff-plan old new (list :owned-buffers (list extra))))))
      (should (plist-get properties :align-diff))
      (should (equal (plist-get properties :owned-buffers) (list extra)))
      (kill-buffer extra))))

(ert-deftest align-diff-test-the-panes-show-the-two-buffers ()
  "End to end: the panes hold each buffer's lines, padded to the same height."
  (align-diff-tests--with-buffers "one\ngone\ntwo\n" "one\ntwo\nadded\n"
    (let ((session (align-diff-buffers old new)))
      (unwind-protect
          (progn
            (should (= (align-buffer-row-count session) 4))
            (should (equal (with-current-buffer (align-buffer-buffer session 'left)
                             (buffer-substring-no-properties (point-min) (point-max)))
                           "one\ngone\ntwo\n\n"))
            (should (equal (with-current-buffer (align-buffer-buffer session 'right)
                             (buffer-substring-no-properties (point-min) (point-max)))
                           "one\n\ntwo\nadded\n")))
        (align-buffer-quit session)))))

(provide 'align-diff-tests)

;;; align-diff-tests.el ends here
