;;; align-diff-difftastic-tests.el --- Tests for the difftastic backend -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   emacs -Q --batch -L . -L test -l test/align-diff-difftastic-tests.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; difftastic itself is run, since what this backend does is read what it
;; says, and what it says of a language is the whole point of using it.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'align-diff)
(require 'align-diff-difftastic)
(require 'align-diff-tests)

(defmacro align-diff-difftastic-tests--with-buffers
    (extension old-text new-text &rest body)
  "Run BODY with `old' and `new' bound to buffers of OLD-TEXT and NEW-TEXT.
Their names end in EXTENSION, which is where difftastic reads the language
off."
  (declare (indent 3))
  `(let ((old (generate-new-buffer (concat "align-diff-old." ,extension)))
         (new (generate-new-buffer (concat "align-diff-new." ,extension))))
     (unwind-protect
         (progn
           (with-current-buffer old
             (insert ,old-text)
             (set-buffer-modified-p nil))
           (with-current-buffer new
             (insert ,new-text)
             (set-buffer-modified-p nil))
           ,@body)
       (kill-buffer old)
       (kill-buffer new))))

(defmacro align-diff-difftastic-tests--with-session
    (extension old-text new-text &rest body)
  "Show OLD-TEXT against NEW-TEXT read by difftastic, and run BODY.
`session' is bound to the session and EXTENSION names the language."
  (declare (indent 3))
  `(align-diff-difftastic-tests--with-buffers ,extension ,old-text ,new-text
     (let* ((align-diff-backend 'difftastic)
            (session (align-diff-buffers old new)))
       (unwind-protect (progn ,@body)
         (align-buffer-quit session)))))

(defun align-diff-difftastic-tests--reading (old new)
  "Return what difftastic reads between buffers OLD and NEW."
  (align-diff-difftastic--reading old new))


;;; What corresponds

(ert-deftest align-diff-difftastic-test-identical-buffers-pair-line-for-line ()
  "Two buffers holding the same text pair every line with its own."
  (align-diff-difftastic-tests--with-buffers "py" "one\ntwo\n" "one\ntwo\n"
    (let ((reading (align-diff-difftastic-tests--reading old new)))
      (should (equal (align-diff-reading-pairs reading) '((1 . 1) (2 . 2))))
      (should (null (align-diff-reading-runs reading))))))

(ert-deftest align-diff-difftastic-test-empty-buffers-pair-nothing ()
  "Two empty buffers have no line to pair."
  (align-diff-difftastic-tests--with-buffers "py" "" ""
    (should (null (align-diff-reading-pairs
                   (align-diff-difftastic-tests--reading old new))))))

(ert-deftest align-diff-difftastic-test-a-changed-line-pairs-with-its-own ()
  "A line edited in place still faces the line it came from."
  (align-diff-difftastic-tests--with-buffers "py" "one\ntwo\n" "one\nTWO\n"
    (should (equal (align-diff-reading-pairs
                    (align-diff-difftastic-tests--reading old new))
                   '((1 . 1) (2 . 2))))))

(ert-deftest align-diff-difftastic-test-an-addition-pairs-nothing-of-its-own ()
  "An added line pairs nothing, and the lines around it pair as they were."
  (align-diff-difftastic-tests--with-buffers
      "py" "one\ntwo\n" "one\nadded\ntwo\n"
    (should (equal (align-diff-reading-pairs
                    (align-diff-difftastic-tests--reading old new))
                   '((1 . 1) (2 . 3))))))

(ert-deftest align-diff-difftastic-test-the-final-newline-alone-pairs-every-line ()
  "A last line that one side ends with a newline is no difference."
  (align-diff-difftastic-tests--with-buffers "py" "one\ntwo" "one\ntwo\n"
    (let ((reading (align-diff-difftastic-tests--reading old new)))
      (should (equal (align-diff-reading-pairs reading) '((1 . 1) (2 . 2))))
      (should (null (align-diff-reading-gaps reading))))))


;;; What is coloured

(ert-deftest align-diff-difftastic-test-only-the-changed-lines-are-coloured ()
  "The lines a change covers are coloured, and the lines around them are not."
  (align-diff-difftastic-tests--with-buffers
      "py" "one\ntwo\nthree\n" "one\nTWO\nthree\n"
    (should (equal (align-diff-reading-runs
                    (align-diff-difftastic-tests--reading old new))
                   '((left 2 1) (right 2 1))))))

(ert-deftest align-diff-difftastic-test-an-addition-colours-the-new-side-alone ()
  "An added line is coloured where it was added and nowhere else."
  (align-diff-difftastic-tests--with-buffers
      "py" "one\ntwo\n" "one\nadded\ntwo\n"
    (should (equal (align-diff-reading-runs
                    (align-diff-difftastic-tests--reading old new))
                   '((right 2 1))))))

(ert-deftest align-diff-difftastic-test-consecutive-changed-lines-are-one-run ()
  "Lines changed one after the other are coloured as one run."
  (align-diff-difftastic-tests--with-buffers
      "py" "one\ntwo\nthree\nfour\n" "one\nTWO\nTHREE\nfour\n"
    (should (equal (align-diff-reading-runs
                    (align-diff-difftastic-tests--reading old new))
                   '((left 2 2) (right 2 2))))))


;;; Where a side has no lines for the other's

(ert-deftest align-diff-difftastic-test-an-addition-marks-the-old-pane ()
  "A line the new side alone has leaves the old pane with nothing there."
  (align-diff-difftastic-tests--with-buffers
      "py" "one\ntwo\n" "one\nadded\ntwo\n"
    (should (equal (align-diff-reading-gaps
                    (align-diff-difftastic-tests--reading old new))
                   '((left 2 added 1 2 t))))))

(ert-deftest align-diff-difftastic-test-a-removal-marks-the-new-pane ()
  "Lines the old side still has leave the new pane with nothing there."
  (align-diff-difftastic-tests--with-buffers
      "py" "one\ntwo\nthree\nfour\n" "one\nfour\n"
    (should (equal (align-diff-reading-gaps
                    (align-diff-difftastic-tests--reading old new))
                   '((right 2 removed 2 2 t))))))

(ert-deftest align-diff-difftastic-test-a-gap-at-the-end-belongs-past-the-last-line ()
  "Lines added at the end belong after every line the old side has."
  (align-diff-difftastic-tests--with-buffers "py" "one\n" "one\ntwo\n"
    (should (equal (align-diff-reading-gaps
                    (align-diff-difftastic-tests--reading old new))
                   '((left 2 added 1 2 t))))))

(ert-deftest align-diff-difftastic-test-a-gap-at-the-top-is-marked-before-line-one ()
  "Lines added at the top belong in front of the first line the old side has."
  (align-diff-difftastic-tests--with-buffers "py" "two\n" "one\ntwo\n"
    (should (equal (align-diff-reading-gaps
                    (align-diff-difftastic-tests--reading old new))
                   '((left 1 added 1 1 t))))))

(ert-deftest align-diff-difftastic-test-an-addition-beside-a-change-is-not-one-sided ()
  "A stretch with lines on both sides is coloured on both and marked on neither.
The pane short of a line is the one the mark would be for, and a reader of that
pane sees the change beside it already."
  (align-diff-difftastic-tests--with-buffers
      "py" "one\ntwo\n" "one\nTWO\nadded\n"
    (should (equal (align-diff-reading-gaps
                    (align-diff-difftastic-tests--reading old new))
                   '((left 3 added 1 3 nil))))))


;;; The stretches the reader moves between

(ert-deftest align-diff-difftastic-test-two-changes-are-two-sections ()
  "Changes with a line both sides agree on between them are two stretches."
  (align-diff-difftastic-tests--with-buffers
      "py" "one\ntwo\nthree\nfour\n" "ONE\ntwo\nthree\nFOUR\n"
    (should (equal (align-diff-reading-sections
                    (align-diff-difftastic-tests--reading old new))
                   '((1 1 1 1) (4 1 4 1))))))

(ert-deftest align-diff-difftastic-test-a-change-and-an-addition-are-one-section ()
  "A change and a line added next to it are the one stretch to move to."
  (align-diff-difftastic-tests--with-buffers
      "py" "one\ntwo\nthree\n" "one\nTWO\nadded\nthree\n"
    (should (equal (align-diff-reading-sections
                    (align-diff-difftastic-tests--reading old new))
                   '((2 1 2 2))))))

(ert-deftest align-diff-difftastic-test-a-section-of-added-lines-counts-none-of-them ()
  "A stretch the old side has no lines of counts zero, in front of the line
they belong before."
  (align-diff-difftastic-tests--with-buffers
      "py" "one\ntwo\n" "one\nadded\ntwo\n"
    (should (equal (align-diff-reading-sections
                    (align-diff-difftastic-tests--reading old new))
                   '((2 0 2 1))))))


;;; Which characters differ

(ert-deftest align-diff-difftastic-test-what-is-marked-is-the-tokens-that-differ ()
  "What is marked inside a changed line is the token difftastic parsed, not the
text a line diff would have left over."
  (align-diff-difftastic-tests--with-buffers
      "py" "x = foo(1, 2)\n" "x = foo(1, 3)\n"
    (should (equal (align-diff-reading-refine
                    (align-diff-difftastic-tests--reading old new))
                   '(ranges (left 1 11 12) (right 1 11 12))))))

(ert-deftest align-diff-difftastic-test-the-marked-characters-count-characters ()
  "A range counts the characters of a line, where difftastic counts its bytes.
A line holding a character of more than one byte would otherwise mark the text
after it one place along."
  (align-diff-difftastic-tests--with-session
      "py" "x = ['caf\N{U+00E9}', 'one']\n" "x = ['caf\N{U+00E9}', 'two']\n"
    (should (equal (align-diff-tests--refinements session 'left) '("'one'")))
    (should (equal (align-diff-tests--refinements session 'right) '("'two'")))))

(ert-deftest align-diff-difftastic-test-refinement-can-be-turned-off ()
  "With `align-diff-refine-hunks' nil the lines are coloured and nothing more."
  (let ((align-diff-refine-hunks nil))
    (align-diff-difftastic-tests--with-session
        "py" "x = foo(1, 2)\n" "x = foo(1, 3)\n"
      (should (null (align-diff-tests--refinements session 'left)))
      (should (equal (align-diff-tests--line-faces
                      (align-buffer-buffer session 'left))
                     '(align-diff-removed))))))


;;; Which language difftastic is told to parse

(ert-deftest align-diff-difftastic-test-the-consumer-names-the-language ()
  "`align-diff-language-extension' is read in front of the buffer's own names.
A buffer holding a revision of a file visits no file, and a name it is given
instead carries the revision, which can end in anything at all."
  (let ((buffer (generate-new-buffer "Scene3D.cpp.~origin/master~")))
    (unwind-protect
        (progn
          (should (null (align-diff-difftastic--extension buffer)))
          (with-current-buffer buffer
            (setq align-diff-language-extension "cpp"))
          (should (equal (align-diff-difftastic--extension buffer) "cpp")))
      (kill-buffer buffer))))

(ert-deftest align-diff-difftastic-test-a-revision-in-the-name-hides-no-extension ()
  "A name ending in a revision between tildes is read past to the extension."
  (let ((buffer (generate-new-buffer "Scene3D.cpp.~HEAD~")))
    (unwind-protect
        (should (equal (align-diff-difftastic--extension buffer) "cpp"))
      (kill-buffer buffer))))

(ert-deftest align-diff-difftastic-test-a-name-that-is-no-extension-is-passed-over ()
  "A name ending in something no file would is left to difftastic's text diff."
  (let ((buffer (generate-new-buffer "Scene3D.cpp<2>")))
    (unwind-protect
        (should (null (align-diff-difftastic--extension buffer)))
      (kill-buffer buffer))))


;;; What the language buys

(ert-deftest align-diff-difftastic-test-the-language-comes-from-the-buffer-name ()
  "Spacing inside an expression is no difference to a language it parses."
  (align-diff-difftastic-tests--with-buffers
      "py" "x = foo(1,2)\n" "x = foo(1, 2)\n"
    (let ((reading (align-diff-difftastic-tests--reading old new)))
      (should (null (align-diff-reading-runs reading)))
      (should (equal (align-diff-reading-pairs reading) '((1 . 1)))))))

(ert-deftest align-diff-difftastic-test-text-is-compared-line-by-line ()
  "The same spacing is a difference where the name names no language to parse."
  (align-diff-difftastic-tests--with-buffers
      "txt" "x = foo(1,2)\n" "x = foo(1, 2)\n"
    (should (equal (align-diff-reading-runs
                    (align-diff-difftastic-tests--reading old new))
                   '((left 1 1) (right 1 1))))))


;;; A side read as empty

(ert-deftest align-diff-difftastic-test-an-emptied-buffer-pairs-nothing ()
  "Every line of a buffer emptied on the new side is coloured, and pairs
nothing."
  (align-diff-difftastic-tests--with-buffers "py" "one\ntwo\n" ""
    (let ((reading (align-diff-difftastic-tests--reading old new)))
      (should (null (align-diff-reading-pairs reading)))
      (should (equal (align-diff-reading-runs reading) '((left 1 2))))
      (should (equal (align-diff-reading-gaps reading)
                     '((right 1 removed 2 1 t)))))))

(ert-deftest align-diff-difftastic-test-a-filled-buffer-pairs-nothing ()
  "Every line written into an empty old side is coloured and pairs nothing."
  (align-diff-difftastic-tests--with-buffers "py" "" "one\ntwo\n"
    (let ((reading (align-diff-difftastic-tests--reading old new)))
      (should (null (align-diff-reading-pairs reading)))
      (should (equal (align-diff-reading-runs reading) '((right 1 2))))
      (should (equal (align-diff-reading-gaps reading)
                     '((left 1 added 2 1 t)))))))


;;; A session of it

(ert-deftest align-diff-difftastic-test-a-session-colours-both-sides ()
  "A session read by difftastic paints the lines of a change in both panes."
  (align-diff-difftastic-tests--with-session
      "py" "one\ntwo\nthree\n" "one\nTWO\nthree\n"
    (should (equal (align-diff-tests--line-faces
                    (align-buffer-buffer session 'left))
                   '(nil align-diff-removed nil)))
    (should (equal (align-diff-tests--line-faces
                    (align-buffer-buffer session 'right))
                   '(nil align-diff-added nil)))))

(ert-deftest align-diff-difftastic-test-a-session-carries-its-backend ()
  "A session reads its buffers again the way it was shown, whatever
`align-diff-backend' says by then."
  (align-diff-difftastic-tests--with-session "py" "one\ntwo\n" "one\nTWO\n"
    (should (eq (align-diff--property session :align-diff-backend) 'difftastic))
    (let ((align-diff-backend 'diff))
      (should (eq (align-diff--session-backend session) 'difftastic)))))

(ert-deftest align-diff-difftastic-test-a-refresh-reads-the-edited-text ()
  "An edit that changes what corresponds is paired and coloured afresh."
  (align-diff-difftastic-tests--with-session "py" "one\ntwo\n" "one\ntwo\n"
    (with-current-buffer (align-buffer-buffer session 'right)
      (goto-char (point-max))
      (insert "three\n"))
    (align-diff-refresh session)
    (should (equal (align-diff-tests--line-faces
                    (align-buffer-buffer session 'right))
                   '(nil nil align-diff-added)))
    (should (equal (align-diff--property session :align-diff-pairs)
                   '((1 . 1) (2 . 2))))))

(ert-deftest align-diff-difftastic-test-a-pad-has-a-row-for-each-missing-line ()
  "Asked for pads, the gap is as many blank rows as the other side has lines."
  (let ((align-diff-gap-marker 'pad))
    (align-diff-difftastic-tests--with-session
        "py" "one\ntwo\n" "one\na\nb\nc\ntwo\n"
      (let* ((old (align-buffer-buffer session 'left))
             (overlay (align-diff-tests--gap-overlay old)))
        (should (equal (align-diff-tests--gaps old) '((2 . pad))))
        (should (= (overlay-get overlay 'align-buffer-pad-rows) 3))
        (should (eq (overlay-get overlay 'align-buffer-pad-facing)
                    (align-buffer-buffer session 'right)))
        (should (= (overlay-get overlay 'align-buffer-pad-facing-line) 2))))))

(ert-deftest align-diff-difftastic-test-the-teardown-takes-the-colours-away ()
  "Quitting a session leaves both buffers as they were."
  (align-diff-difftastic-tests--with-buffers "py" "one\ntwo\n" "one\nTWO\n"
    (let* ((align-diff-backend 'difftastic)
           (session (align-diff-buffers old new)))
      (align-buffer-quit session)
      (should (null (align-diff-tests--overlays old)))
      (should (null (align-diff-tests--overlays new)))
      (should-not (buffer-modified-p old))
      (should-not (buffer-modified-p new)))))

(provide 'align-diff-difftastic-tests)

;;; align-diff-difftastic-tests.el ends here
