;;; align-diff-tests.el --- Tests for align-diff -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   emacs -Q --batch -L . -l test/align-diff-tests.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; The tests run `diff-command' for real, since what align-diff does is read
;; its output.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'align-diff)

(defmacro align-diff-tests--with-buffers (old-text new-text &rest body)
  "Run BODY with `old' and `new' bound to buffers holding OLD-TEXT and NEW-TEXT."
  (declare (indent 2))
  `(let ((old (generate-new-buffer " *align-diff test old*"))
         (new (generate-new-buffer " *align-diff test new*")))
     (unwind-protect
         (progn
           ;; Left unmodified, the way a buffer the reader is looking at is,
           ;; so that a test can say align-diff touched no text.
           (with-current-buffer old
             (insert ,old-text)
             (set-buffer-modified-p nil))
           (with-current-buffer new
             (insert ,new-text)
             (set-buffer-modified-p nil))
           ,@body)
       (kill-buffer old)
       (kill-buffer new))))

(defmacro align-diff-tests--with-session (old-text new-text &rest body)
  "Show OLD-TEXT against NEW-TEXT and run BODY with `session' bound."
  (declare (indent 2))
  `(align-diff-tests--with-buffers ,old-text ,new-text
     (let ((session (align-diff-buffers old new)))
       (unwind-protect (progn ,@body)
         (align-buffer-quit session)))))

(defun align-diff-tests--pairs (old new)
  "Return the pairs align-diff reads between buffers OLD and NEW."
  (align-diff--pairs (align-diff--hunks old new) old new))

(defun align-diff-tests--overlays (buffer)
  "Return the overlays align-diff put in BUFFER, in order."
  (with-current-buffer buffer
    (sort (seq-filter (lambda (overlay) (overlay-get overlay 'align-diff))
                      (overlays-in (point-min) (point-max)))
          (lambda (one other) (< (overlay-start one) (overlay-start other))))))

(defun align-diff-tests--line-faces (buffer)
  "Return the face align-diff put on each line of BUFFER, in order."
  (with-current-buffer buffer
    (save-excursion
      (let ((faces nil))
        (dotimes (index (count-lines (point-min) (point-max)))
          (goto-char (point-min))
          (forward-line index)
          (push (cl-some (lambda (overlay)
                           (and (overlay-get overlay 'align-diff)
                                (not (overlay-get overlay
                                                  'align-diff-refinement))
                                (not (overlay-get overlay 'align-diff-gap))
                                (overlay-get overlay 'face)))
                         (overlays-at (point)))
                faces))
        (nreverse faces)))))

(defun align-diff-tests--gaps (buffer)
  "Return the gap marks in BUFFER, as (LINE . KIND), one per line marked.
A slit takes two overlays on the one line and a fringe triangle one on each
side of the boundary, so the lines are what these count."
  (let ((marks nil))
    (dolist (overlay (align-diff-tests--gap-overlays buffer))
      ;; A pad hangs off the newline of the line before the boundary, and the
      ;; line it marks is the one its rows are drawn in front of.
      (let ((line (align-buffer-line-at
                   buffer (if (overlay-get overlay 'align-buffer-pad-rows)
                              (align-buffer-pad-anchor overlay)
                            (overlay-start overlay))))
            (kind (align-diff-tests--gap-kind overlay)))
        (cond ((null kind))             ; the newline ending the line's own row
              ((assoc line marks) (setcdr (assoc line marks) kind))
              (t (push (cons line kind) marks)))))
    (nreverse marks)))

(defun align-diff-tests--gap-face (buffer)
  "Return the face BUFFER's first fringe mark is drawn in."
  (nth 2 (get-text-property 0 'display
                            (overlay-get (align-diff-tests--gap-overlay buffer)
                                         'before-string))))

(defun align-diff-tests--gap-overlays (buffer)
  "Return BUFFER's gap marks, in order."
  (seq-filter (lambda (overlay) (overlay-get overlay 'align-diff-gap))
              (align-diff-tests--overlays buffer)))

(defun align-diff-tests--gap-overlay (buffer)
  "Return the one gap mark in BUFFER."
  (car (align-diff-tests--gap-overlays buffer)))

(defun align-diff-tests--gap-kind (overlay)
  "Return which kind of mark OVERLAY draws, or nil when it draws nothing."
  (let ((face (overlay-get overlay 'face))
        (string (overlay-get overlay 'before-string)))
    (cond ((overlay-get overlay 'align-buffer-pad-rows) 'pad)
          ((plist-get face :height) 'slit)
          ((null string) nil)
          ((memq (car-safe (get-text-property 0 'display string))
                 '(left-fringe right-fringe))
           'fringe)
          ((plist-get (get-text-property 0 'face string) :height) 'slit)
          (t nil))))

(defun align-diff-tests--refinements (session side)
  "Return the text of SESSION's refinement overlays on SIDE, in order."
  (let ((buffer (align-buffer-buffer session side)))
    (with-current-buffer buffer
      (mapcar (lambda (overlay)
                (buffer-substring-no-properties (overlay-start overlay)
                                                (overlay-end overlay)))
              (seq-filter (lambda (overlay)
                            (overlay-get overlay 'align-diff-refinement))
                          (align-diff-tests--overlays buffer))))))


;;; Reading the hunk headers

(ert-deftest align-diff-test-a-header-without-a-count-means-one-line ()
  "A range of one line is written without its count."
  (should (equal (align-diff--hunks-in "@@ -30 +42 @@\n") '((30 1 42 1)))))

(ert-deftest align-diff-test-a-header-with-counts-reads-both-ranges ()
  "Both sides of a header give a line and a count."
  (should (equal (align-diff--hunks-in "@@ -10,2 +12,3 @@\n") '((10 2 12 3)))))

(ert-deftest align-diff-test-a-count-of-zero-moves-on-to-the-first-line ()
  "A side with no lines carries the line before the gap, which is one too few."
  (should (equal (align-diff--hunks-in "@@ -1,0 +2,2 @@\n") '((2 0 2 2))))
  (should (equal (align-diff--hunks-in "@@ -2,2 +1,0 @@\n") '((2 2 2 0)))))

(ert-deftest align-diff-test-the-reader-takes-only-the-headers ()
  "The command line, the labels, the body and the trailer are all passed over."
  (should (equal (align-diff--hunks-in
                  (concat "diff -U0 --label a --label b /tmp/one /tmp/two\n"
                          "--- a\n"
                          "+++ b\n"
                          "@@ -2 +2 @@\n"
                          "-two\n"
                          "+TWO\n"
                          "\\ No newline at end of file\n"
                          "\n"
                          "Diff finished.\n"))
                 '((2 1 2 1)))))


;;; What corresponds

(ert-deftest align-diff-test-identical-buffers-pair-line-for-line ()
  "Two buffers holding the same text pair every line with its own."
  (align-diff-tests--with-buffers "one\ntwo\n" "one\ntwo\n"
    (should (equal (align-diff-tests--pairs old new) '((1 . 1) (2 . 2))))))

(ert-deftest align-diff-test-empty-buffers-pair-nothing ()
  "Two empty buffers have no line to pair."
  (align-diff-tests--with-buffers "" ""
    (should (equal (align-diff-tests--pairs old new) nil))))

(ert-deftest align-diff-test-a-changed-line-pairs-with-its-own ()
  "A line edited in place still faces the line it came from."
  (align-diff-tests--with-buffers "one\ntwo\n" "one\nTWO\n"
    (should (equal (align-diff-tests--pairs old new) '((1 . 1) (2 . 2))))))

(ert-deftest align-diff-test-an-addition-pairs-nothing-of-its-own ()
  "A line only the new side has has no counterpart, and shifts what follows."
  (align-diff-tests--with-buffers "one\ntwo\n" "one\nextra\ntwo\n"
    (should (equal (align-diff-tests--pairs old new) '((1 . 1) (2 . 3))))))

(ert-deftest align-diff-test-a-removal-pairs-nothing-of-its-own ()
  "A line only the old side has has no counterpart."
  (align-diff-tests--with-buffers "one\ngone\ntwo\n" "one\ntwo\n"
    (should (equal (align-diff-tests--pairs old new) '((1 . 1) (3 . 2))))))

(ert-deftest align-diff-test-an-addition-at-the-top-shifts-every-line ()
  "A line added above everything leaves the first pair offset by one."
  (align-diff-tests--with-buffers "a\nb\n" "x\na\nb\n"
    (should (equal (align-diff-tests--pairs old new) '((1 . 2) (2 . 3))))))

(ert-deftest align-diff-test-an-addition-at-the-end-pairs-what-came-before ()
  "A line added below everything leaves the lines before it paired."
  (align-diff-tests--with-buffers "a\nb\n" "a\nb\nx\n"
    (should (equal (align-diff-tests--pairs old new) '((1 . 1) (2 . 2))))))

(ert-deftest align-diff-test-an-uneven-hunk-pairs-as-far-as-the-shorter-side ()
  "Three lines replaced by one: the first pair up, the other two stand alone."
  (align-diff-tests--with-buffers "a\nb\nc\nd\ne\n" "a\nB\ne\n"
    (should (equal (align-diff-tests--pairs old new)
                   '((1 . 1) (2 . 2) (5 . 3))))))

(ert-deftest align-diff-test-a-longer-new-side-pairs-from-the-top-of-the-hunk ()
  "One line replaced by three: the first pair up, the other two stand alone."
  (align-diff-tests--with-buffers "a\nb\ne\n" "a\nB\nC\nD\ne\n"
    (should (equal (align-diff-tests--pairs old new)
                   '((1 . 1) (2 . 2) (3 . 5))))))

(ert-deftest align-diff-test-an-emptied-buffer-pairs-nothing ()
  "A new side with no lines at all leaves every old line unpaired."
  (align-diff-tests--with-buffers "a\nb\n" ""
    (should (equal (align-diff-tests--pairs old new) nil))))

(ert-deftest align-diff-test-a-last-line-without-a-newline-is-still-a-line ()
  "A buffer whose last line has no newline pairs that line like any other."
  (align-diff-tests--with-buffers "one\ntwo" "one\nTWO"
    (should (equal (align-diff-tests--pairs old new) '((1 . 1) (2 . 2))))))

(ert-deftest align-diff-test-the-final-newline-alone-pairs-every-line ()
  "A side that ends without a newline where the other ends with one still pairs."
  (align-diff-tests--with-buffers "a\nb\n" "a\nb"
    (should (equal (align-diff-tests--pairs old new) '((1 . 1) (2 . 2))))))


;;; Colouring the lines

(ert-deftest align-diff-test-a-changed-line-is-coloured-on-both-sides ()
  "The old side of a change reads as removed and the new side as added."
  (align-diff-tests--with-session "one\ntwo\n" "one\nTWO\n"
    (should (equal (align-diff-tests--line-faces old)
                   '(nil align-diff-removed)))
    (should (equal (align-diff-tests--line-faces new)
                   '(nil align-diff-added)))))

(ert-deftest align-diff-test-an-addition-colours-the-new-side-alone ()
  "A line only the new side has is coloured there and nowhere else."
  (align-diff-tests--with-session "one\ntwo\n" "one\nextra\ntwo\n"
    (should (equal (align-diff-tests--line-faces old) '(nil nil)))
    (should (equal (align-diff-tests--line-faces new)
                   '(nil align-diff-added nil)))))

(ert-deftest align-diff-test-a-removal-colours-the-old-side-alone ()
  "A line only the old side has is coloured there and nowhere else."
  (align-diff-tests--with-session "one\ngone\ntwo\n" "one\ntwo\n"
    (should (equal (align-diff-tests--line-faces old)
                   '(nil align-diff-removed nil)))
    (should (equal (align-diff-tests--line-faces new) '(nil nil)))))

(ert-deftest align-diff-test-an-unpaired-line-of-a-hunk-is-coloured-too ()
  "Every line of a hunk is coloured, paired or not."
  (align-diff-tests--with-session "a\nb\nc\nd\ne\n" "a\nB\ne\n"
    (should (equal (align-diff-tests--line-faces old)
                   '(nil align-diff-removed align-diff-removed
                         align-diff-removed nil)))
    (should (equal (align-diff-tests--line-faces new)
                   '(nil align-diff-added nil)))))

(ert-deftest align-diff-test-identical-buffers-are-not-coloured ()
  "Nothing differs, so nothing is coloured."
  (align-diff-tests--with-session "one\ntwo\n" "one\ntwo\n"
    (should (equal (align-diff-tests--line-faces old) '(nil nil)))
    (should (equal (align-diff-tests--line-faces new) '(nil nil)))))

(ert-deftest align-diff-test-a-colour-reaches-past-the-end-of-the-line ()
  "The overlay covers the newline, which is what lets the face extend."
  (align-diff-tests--with-session "one\ntwo\n" "one\nTWO\n"
    (with-current-buffer new
      (let ((overlay (cl-find-if-not
                      (lambda (one) (overlay-get one 'align-diff-refinement))
                      (align-diff-tests--overlays new))))
        (should (= (overlay-end overlay) (point-max)))))))

(ert-deftest align-diff-test-a-trailing-carriage-return-is-no-difference ()
  "The default switches ignore one, so a CRLF side against an LF side is clean.
Reviewing a CRLF file against an LF revision otherwise colours every line."
  (align-diff-tests--with-session "one\r\ntwo\r\n" "one\ntwo\n"
    (should (equal (align-diff-tests--line-faces old) '(nil nil))))
  (let ((align-diff-switches nil))
    (align-diff-tests--with-session "one\r\ntwo\r\n" "one\ntwo\n"
      (should (equal (align-diff-tests--line-faces old)
                     '(align-diff-removed align-diff-removed))))))


;;; Marking a gap

(ert-deftest align-diff-test-an-addition-marks-the-old-pane ()
  "Lines only the new side has leave the old pane a mark where they would be."
  (align-diff-tests--with-session "one\ntwo\n" "one\nextra\ntwo\n"
    (should (equal (align-diff-tests--gaps old) '((1 . slit))))
    (should (null (align-diff-tests--gaps new)))))

(ert-deftest align-diff-test-a-removal-marks-the-new-pane ()
  "Lines only the old side has leave the new pane a mark."
  (align-diff-tests--with-session "one\ngone\ntwo\n" "one\ntwo\n"
    (should (equal (align-diff-tests--gaps new) '((1 . slit))))
    (should (null (align-diff-tests--gaps old)))))

(ert-deftest align-diff-test-an-uneven-hunk-is-not-marked ()
  "Three lines against one: both sides carry colour there already, so the two
lines left over at the end of the old side are not marked in the new pane."
  (align-diff-tests--with-session "a\nb\nc\nd\ne\n" "a\nB\ne\n"
    (should (null (align-diff-tests--gaps new)))
    (should (null (align-diff-tests--gaps old)))))

(ert-deftest align-diff-test-a-hunk-of-equal-sides-is-not-marked ()
  "Every line of the hunk has a counterpart, so no lines are missing anywhere."
  (align-diff-tests--with-session "one\ntwo\n" "one\nTWO\n"
    (should (null (align-diff-tests--gaps old)))
    (should (null (align-diff-tests--gaps new)))))

(ert-deftest align-diff-test-a-slit-leaves-the-line-above-its-own-row ()
  "A plain newline ends that line's row, so it keeps it and its line number.
The slit is then the row the line's own newline ends, drawn at the height that
newline is given."
  (align-diff-tests--with-session "one\ngone\ntwo\n" "one\ntwo\n"
    (let* ((marks (align-diff-tests--gap-overlays new))
           (breaker (seq-find (lambda (one) (overlay-get one 'after-string))
                              marks))
           (slit (seq-find (lambda (one) (overlay-get one 'face)) marks))
           (end (align-diff--line-end new 1)))
      (should (equal (overlay-get breaker 'after-string) "\n"))
      (should (null (get-text-property 0 'face
                                       (overlay-get breaker 'after-string))))
      (should (= (overlay-start slit) end))
      (should (= (overlay-end slit) (1+ end)))
      (should (= (plist-get (overlay-get slit 'face) :height)
                 align-diff-slit-height)))))

(ert-deftest align-diff-test-a-gap-above-the-first-line-is-marked-before-it ()
  "Nothing above the gap to hang the slit on, so it goes before the text."
  (align-diff-tests--with-session "one\ntwo\n" "added\none\ntwo\n"
    (should (equal (align-diff-tests--gaps old) '((1 . slit))))
    (should (overlay-get (align-diff-tests--gap-overlay old) 'before-string))))

(ert-deftest align-diff-test-a-gap-at-the-end-is-marked-under-the-last-line ()
  "A hunk past the last line is marked below it, not before it."
  (align-diff-tests--with-session "a\nb\nc\n" "a\n"
    (should (equal (align-diff-tests--gaps new) '((1 . slit))))))

(ert-deftest align-diff-test-the-fringe-mark-straddles-the-boundary ()
  "Half the triangle under the line above and half over the line below, so its
tip falls between the two rows."
  (let ((align-diff-gap-marker 'fringe))
    (align-diff-tests--with-session "one\ntwo\n" "one\nextra\ntwo\n"
      (should (equal (align-diff-tests--gaps old)
                     '((1 . fringe) (2 . fringe))))
      (let ((halves (mapcar (lambda (overlay)
                              (nth 1 (get-text-property
                                      0 'display
                                      (overlay-get overlay 'before-string))))
                            (align-diff-tests--gap-overlays old))))
        (should (equal halves
                       '(align-diff-gap-upper align-diff-gap-lower)))))))

(ert-deftest align-diff-test-a-triangle-marks-a-one-sided-hunk-alone-too ()
  "A hunk whose lines changed on both sides is coloured on both, so the lines
left over at the end of the longer side get no triangle of their own."
  (let ((align-diff-gap-marker 'fringe))
    (align-diff-tests--with-session "a\nb\nc\nd\ne\n" "a\nB\ne\n"
      (should (null (align-diff-tests--gaps new)))
      (should (null (align-diff-tests--gaps old))))))

(ert-deftest align-diff-test-the-fringe-mark-takes-no-room-in-the-text ()
  "The bitmap is drawn in the fringe that is asked for, beside the text."
  (let ((align-diff-gap-marker 'fringe)
        (align-diff-gap-fringe 'right-fringe))
    (align-diff-tests--with-session "one\ngone\ntwo\n" "one\ntwo\n"
      (let ((display (get-text-property
                      0 'display
                      (overlay-get (align-diff-tests--gap-overlay new)
                                   'before-string))))
        (should (eq (nth 0 display) 'right-fringe))))))

(ert-deftest align-diff-test-a-mark-says-which-way-the-lines-went ()
  "A gap facing lines gained reads as gained, and one facing lines lost as lost."
  (let ((align-diff-gap-marker 'fringe))
    (align-diff-tests--with-session "one\ntwo\n" "one\nextra\ntwo\n"
      (should (eq (align-diff-tests--gap-face old) 'align-diff-gap-added)))
    (align-diff-tests--with-session "one\ngone\ntwo\n" "one\ntwo\n"
      (should (eq (align-diff-tests--gap-face new)
                  'align-diff-gap-removed)))))

(ert-deftest align-diff-test-a-pad-has-a-row-for-each-missing-line ()
  "Asked for pads, the gap is as many blank rows as the other side has lines."
  (let ((align-diff-gap-marker 'pad))
    (align-diff-tests--with-session "one\ntwo\n" "one\na\nb\nc\ntwo\n"
      (should (equal (align-diff-tests--gaps old) '((2 . pad))))
      (let ((overlay (align-diff-tests--gap-overlay old)))
        (should (= (overlay-get overlay 'align-buffer-pad-rows) 3))
        (should (eq (overlay-get overlay 'align-buffer-pad-facing) new))
        (should (= (overlay-get overlay 'align-buffer-pad-facing-line) 2))))))

(ert-deftest align-diff-test-a-pad-leaves-a-coloured-line-painted ()
  "The newline closing the row of the line a pad hangs after keeps its colour.
What paints a row to the edge of the window is the face of the newline that
closes it, and a pad hanging off a changed line closes that line's row itself.
Faced with nothing there, the line would be painted only as far as its text."
  (let ((align-diff-gap-marker 'pad))
    (align-diff-tests--with-session "one\nchanged\ntwo\n"
        "one\nCHANGED\na\nb\ntwo\n"
      (let ((overlay (align-diff-tests--gap-overlay old)))
        (should (= (align-buffer-pad-rows overlay) 2))
        (should (eq (get-text-property 0 'face
                                       (overlay-get overlay 'before-string))
                    'align-diff-removed))
        (should (eq (overlay-get overlay 'align-buffer-pad-opening)
                    'align-diff-removed))))
    ;; And an unchanged line keeps its row as the reader wrote it.
    (align-diff-tests--with-session "one\ntwo\n" "one\na\nb\ntwo\n"
      (should-not (get-text-property
                   0 'face (overlay-get (align-diff-tests--gap-overlay old)
                                        'before-string))))))

(ert-deftest align-diff-test-a-pad-of-a-diff-is-walked-row-by-row ()
  "The rows a gap is drawn as are rows a session steps through one at a time.
The stepping itself is align-buffer's; what is checked here is that a pad of
ours is a pad it recognises."
  (let ((align-diff-gap-marker 'pad))
    (align-diff-tests--with-session "one\ntwo\n" "one\na\nb\nc\ntwo\n"
      (with-current-buffer old
        (goto-char (align-buffer-line-position old 1))
        (call-interactively #'next-line)
        (should (eq (car align-buffer--walked)
                    (align-diff-tests--gap-overlay old)))
        (should (equal (cdr align-buffer--walked) 0))
        (call-interactively #'next-line)
        (should (equal (cdr align-buffer--walked) 1))
        (should (= (line-number-at-pos (point)) 1))))))

(ert-deftest align-diff-test-a-hunk-is-a-section-of-the-session ()
  "The hunks are what the session gives align-buffer to navigate between.
A hunk with lines on one side only counts none on the other, which is how a
section says the side has them nowhere."
  (align-diff-tests--with-session
      "one\ntwo\nthree\nfour\nfive\n"
      "one\nadded\ntwo\nthree\nCHANGED\nfive\n"
    (should (equal (align-buffer-session-sections session)
                   '((2 0 2 1) (4 1 5 1))))
    (with-current-buffer new
      (goto-char (point-max))
      (insert "six\n"))
    (align-diff-refresh session)
    (should (equal (align-buffer-session-sections session)
                   '((2 0 2 1) (4 1 5 1) (6 0 7 1))))))

(ert-deftest align-diff-test-section-navigation-moves-hunk-to-hunk ()
  "Moving section to section takes the reader from one hunk to the next.
The moving itself is align-buffer's; what is checked here is that a hunk of
ours is a section it can move to, from either pane, the pad of a one-sided
hunk included."
  (let ((align-diff-gap-marker 'pad))
    (align-diff-tests--with-session
        "one\ntwo\nthree\nfour\nfive\n"
        "one\nadded\ntwo\nthree\nCHANGED\nfive\n"
      (with-selected-window (get-buffer-window new)
        (goto-char (point-min))
        (call-interactively #'align-buffer-next-section)
        (should (= (line-number-at-pos (point)) 2))
        (call-interactively #'align-buffer-next-section)
        (should (= (line-number-at-pos (point)) 5)))
      (with-selected-window (get-buffer-window old)
        (goto-char (point-min))
        ;; The old pane has none of the added line, so what it stops on is the
        ;; first row of the pad standing for it.
        (call-interactively #'align-buffer-next-section)
        (should (eq (car align-buffer--walked)
                    (align-diff-tests--gap-overlay old)))
        (should (equal (cdr align-buffer--walked) 0))
        (call-interactively #'align-buffer-next-section)
        (should (null align-buffer--walked))
        (should (= (line-number-at-pos (point)) 4))
        (should-error (align-buffer-next-section) :type 'user-error)))))

(ert-deftest align-diff-test-a-mark-takes-no-position-in-the-buffer ()
  "The marks are display, so the text is the reader's own, untouched."
  (align-diff-tests--with-session "one\ngone\ntwo\n" "one\ntwo\n"
    (should (equal (with-current-buffer new (buffer-string)) "one\ntwo\n"))
    (should-not (buffer-modified-p new))))

(ert-deftest align-diff-test-the-marks-can-be-turned-off ()
  "With `align-diff-gap-marker' nil only the lines are coloured."
  (let ((align-diff-gap-marker nil))
    (align-diff-tests--with-session "one\ngone\ntwo\n" "one\ntwo\n"
      (should (null (align-diff-tests--gaps new)))
      (should (equal (align-diff-tests--line-faces old)
                     '(nil align-diff-removed nil))))))

(ert-deftest align-diff-test-a-refresh-marks-the-edited-text ()
  "The marks follow an edit like the colours do."
  (align-diff-tests--with-session "one\ntwo\n" "one\ntwo\n"
    (should (null (align-diff-tests--gaps old)))
    (with-current-buffer new
      (goto-char (point-max))
      (insert "three\nfour\n"))
    (align-diff-refresh session)
    (should (equal (align-diff-tests--gaps old) '((2 . slit))))))


;;; Colouring the characters

(ert-deftest align-diff-test-refinement-marks-what-changed ()
  "A changed line has the characters that differ marked on each side."
  (align-diff-tests--with-session "int foo(int bar);\n" "int foo(int baz);\n"
    (should (equal (align-diff-tests--refinements session 'left) '("bar")))
    (should (equal (align-diff-tests--refinements session 'right) '("baz")))))

(ert-deftest align-diff-test-refinement-follows-a-line-split ()
  "A hunk is refined whole, so text moved to a new line still corresponds.
Refining line by line would call the second argument removed on one line and
added on another, when all that happened is a line break."
  (align-diff-tests--with-session
      "one\ncall(first, second);\nthree\n"
      "one\ncall(first,\n     second_renamed);\nthree\n"
    (should (equal (align-diff-tests--refinements session 'left) nil))
    (should (equal (align-diff-tests--refinements session 'right)
                   '("_renamed")))))

(ert-deftest align-diff-test-a-one-sided-hunk-is-not-refined ()
  "A hunk that is all insertion has nothing to correspond with."
  (align-diff-tests--with-session "one\ntwo\n" "one\nextra\ntwo\n"
    (should (null (align-diff-tests--refinements session 'left)))
    (should (null (align-diff-tests--refinements session 'right)))))

(ert-deftest align-diff-test-refinement-can-be-turned-off ()
  "With `align-diff-refine-hunks' nil only the lines are coloured."
  (let ((align-diff-refine-hunks nil))
    (align-diff-tests--with-session "int foo(int bar);\n" "int foo(int baz);\n"
      (should (null (align-diff-tests--refinements session 'left)))
      (should (equal (align-diff-tests--line-faces old)
                     '(align-diff-removed))))))

(ert-deftest align-diff-test-refinement-leaves-point-where-it-was ()
  "Refining walks both buffers, and gives each its point back."
  (align-diff-tests--with-buffers "int foo(int bar);\n" "int foo(int baz);\n"
    (with-current-buffer old (goto-char (point-min)))
    (with-current-buffer new (goto-char (point-min)))
    (let ((session (align-diff-buffers old new)))
      (unwind-protect
          (progn
            (should (= (with-current-buffer old (point)) (point-min)))
            (should (= (with-current-buffer new (point)) (point-min))))
        (align-buffer-quit session)))))


;;; Keeping up with the reader's edits

(ert-deftest align-diff-test-a-refresh-reads-the-edited-text ()
  "An edit that changes what corresponds is paired and coloured afresh."
  (align-diff-tests--with-session "one\ntwo\n" "one\ntwo\n"
    (with-current-buffer new
      (goto-char (point-max))
      (insert "three\n"))
    (align-diff-refresh session)
    (should (equal (align-diff-tests--line-faces new)
                   '(nil nil align-diff-added)))
    (should (equal (align-diff--property session :align-diff-pairs)
                   '((1 . 1) (2 . 2))))))

(ert-deftest align-diff-test-a-refresh-marks-an-edit-inside-a-changed-line ()
  "An edit that leaves the pairing alone is marked afresh where it stands."
  (align-diff-tests--with-session "one\ntwo\n" "one\nTWO\n"
    (with-current-buffer new
      (goto-char (point-max))
      (forward-line -1)
      (end-of-line)
      (insert "X"))
    (align-diff-refresh session)
    (should (equal (align-diff--property session :align-diff-pairs)
                   '((1 . 1) (2 . 2))))
    (should (equal (align-diff-tests--refinements session 'right) '("TWOX")))))

(ert-deftest align-diff-test-the-idle-look-waits-for-a-change ()
  "Nothing is diffed again while the text is as it was."
  (align-diff-tests--with-session "one\ntwo\n" "one\nTWO\n"
    (let ((pairs (align-diff--property session :align-diff-pairs)))
      (align-diff--idle session)
      (should (eq (align-diff--property session :align-diff-pairs) pairs)))))

(ert-deftest align-diff-test-the-idle-look-diffs-an-edited-session-again ()
  "A change to either buffer is picked up by the idle look."
  (align-diff-tests--with-session "one\ntwo\n" "one\ntwo\n"
    (with-current-buffer new
      (goto-char (point-min))
      (insert "added\n"))
    (align-diff--idle session)
    (should (equal (align-diff--property session :align-diff-pairs)
                   '((1 . 2) (2 . 3))))
    (should (equal (align-diff-tests--line-faces new)
                   '(align-diff-added nil nil)))))

(ert-deftest align-diff-test-the-watch-can-be-turned-off ()
  "With `align-diff-idle-delay' nil no timer is set."
  (let ((align-diff-idle-delay nil))
    (align-diff-tests--with-session "one\ntwo\n" "one\nTWO\n"
      (should (null (align-diff--property session :align-diff-timer))))))

(ert-deftest align-diff-test-the-watch-ends-with-the-session ()
  "The timer is cancelled when the session is taken apart."
  (align-diff-tests--with-buffers "one\ntwo\n" "one\nTWO\n"
    (let* ((session (align-diff-buffers old new))
           (timer (align-diff--property session :align-diff-timer)))
      (should (memq timer timer-idle-list))
      (align-buffer-quit session)
      (should-not (memq timer timer-idle-list)))))


;;; Taking it apart

(ert-deftest align-diff-test-the-teardown-takes-the-colours-away ()
  "Both buffers are left with none of align-diff's overlays in them.
A hunk of two lines against one, so there is a colour, a mark and a refinement
to be rid of."
  (align-diff-tests--with-buffers "int foo(int bar);\ngone\n"
      "int foo(int baz);\n"
    (align-buffer-quit (align-diff-buffers old new))
    (should (null (align-diff-tests--overlays old)))
    (should (null (align-diff-tests--overlays new)))))

(ert-deftest align-diff-test-what-a-caller-passes-reaches-the-session ()
  "A caller's properties are carried beside align-diff's own mark."
  (align-diff-tests--with-buffers "one\ntwo\n" "one\nTWO\n"
    (let* ((extra (generate-new-buffer " *align-diff owned*"))
           (session (align-diff-buffers old new
                                        (list :owned-buffers (list extra)))))
      (should (align-diff-session-p session))
      (should (equal (align-diff--property session :owned-buffers)
                     (list extra)))
      (align-buffer-quit session)
      (should-not (buffer-live-p extra)))))

(provide 'align-diff-tests)

;;; align-diff-tests.el ends here
