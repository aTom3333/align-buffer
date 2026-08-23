;;; align-buffer-xref-tests.el --- Tests for align-buffer-xref -*- lexical-binding: t; -*-

;;; Commentary:

;; The source buffers carry a backend of our own making, which records where it
;; was asked and answers with whatever the test wants.  So these test the
;; forwarding rather than any real backend's behaviour, and need no server.
;;
;; Run with:
;;   emacs -Q --batch -L . -L extensions -l test/align-buffer-xref-tests.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'align-buffer-xref)

(defvar align-buffer-xref-tests--asked nil
  "Each question the fake backend was asked, as (METHOD BUFFER POSITION).")

(defvar align-buffer-xref-tests--answer nil
  "What the fake backend answers `xref-backend-definitions' with.")

(defun align-buffer-xref-tests--record (method)
  (push (list method (current-buffer) (point)) align-buffer-xref-tests--asked))

(cl-defmethod xref-backend-identifier-at-point
  ((_backend (eql align-buffer-xref-tests)))
  (align-buffer-xref-tests--record 'identifier)
  "the-identifier")

(cl-defmethod xref-backend-definitions
  ((_backend (eql align-buffer-xref-tests)) identifier)
  (align-buffer-xref-tests--record (list 'definitions identifier))
  align-buffer-xref-tests--answer)

(defmacro align-buffer-xref-tests--with-session (&rest body)
  "Run BODY with `session', `old' and `new' bound, both sources answering.
The two sources hold the same three lines, so a row reads the same line number
on either side and the test can tell the panes apart by their text."
  (declare (indent 0))
  `(let ((old (generate-new-buffer " *xref test old*"))
         (new (generate-new-buffer " *xref test new*"))
         (align-buffer-xref-tests--asked nil))
     (unwind-protect
         (progn
           (dolist (buffer (list old new))
             (with-current-buffer buffer
               (insert "int first(int a);\nint second(int b);\nint third(int c);\n")
               (add-hook 'xref-backend-functions
                         (lambda () 'align-buffer-xref-tests) nil t)))
           (let ((session
                  (align-buffer-show
                   (align-buffer-plan-create
                    :left-name " *xref left*"
                    :right-name " *xref right*"
                    :rows (vector
                           (align-buffer-row-create
                            :left (align-buffer-cell-create
                                   :kind 'line :source old :line 1)
                            :right (align-buffer-cell-create
                                    :kind 'line :source new :line 1))
                           (align-buffer-row-create
                            :left (align-buffer-cell-create :kind 'blank)
                            :right (align-buffer-cell-create
                                    :kind 'line :source new :line 2))
                           (align-buffer-row-create
                            :left (align-buffer-cell-create
                                   :kind 'line :source old :line 2)
                            :right (align-buffer-cell-create
                                    :kind 'line :source new :line 3)))))))
             (unwind-protect (progn ,@body)
               (align-buffer-quit session))))
       (kill-buffer old)
       (kill-buffer new))))

(defun align-buffer-xref-tests--goto (session side row column)
  "Put point in SESSION's SIDE pane, on ROW, COLUMN characters in."
  (set-buffer (align-buffer-buffer session side))
  (goto-char (+ (align-buffer-row-beginning-position session row side) column)))

(ert-deftest align-buffer-xref-test-a-pane-offers-the-backend ()
  "Both panes answer xref, and only where a row stands for a source line."
  (align-buffer-xref-tests--with-session
    (align-buffer-xref-tests--goto session 'left 0 4)
    (should (eq (xref-find-backend) 'align-buffer-xref))
    (align-buffer-xref-tests--goto session 'right 1 4)
    (should (eq (xref-find-backend) 'align-buffer-xref))
    ;; Row one is padding on the left, and padding stands for nothing.
    (align-buffer-xref-tests--goto session 'left 1 0)
    (should-not (align-buffer-xref-backend))))

(ert-deftest align-buffer-xref-test-the-question-reaches-the-source ()
  "The source's own backend is asked, in the source, at the translated point."
  (align-buffer-xref-tests--with-session
    ;; Row two of the right pane shows line three of the new buffer; column 4 of
    ;; the row is column 4 of that line.
    (align-buffer-xref-tests--goto session 'right 2 4)
    (should (equal (xref-backend-identifier-at-point 'align-buffer-xref)
                   "the-identifier"))
    (pcase-let ((`(,method ,buffer ,position)
                 (car align-buffer-xref-tests--asked)))
      (should (eq method 'identifier))
      (should (eq buffer new))
      (should (equal (with-current-buffer new
                       (list (line-number-at-pos position)
                             (- position (save-excursion
                                           (goto-char position)
                                           (line-beginning-position)))))
                     '(3 4))))))

(ert-deftest align-buffer-xref-test-a-column-past-the-line-clamps ()
  "A column the source line does not reach lands at its end, not on the next."
  (align-buffer-xref-tests--with-session
    (align-buffer-xref-tests--goto session 'left 0 0)
    (goto-char (line-end-position))
    (xref-backend-identifier-at-point 'align-buffer-xref)
    (pcase-let ((`(,_ ,buffer ,position) (car align-buffer-xref-tests--asked)))
      (should (eq buffer old))
      (should (equal (with-current-buffer old
                       (save-excursion
                         (goto-char position)
                         (buffer-substring-no-properties (line-beginning-position)
                                                         (point))))
                     "int first(int a);")))))

(ert-deftest align-buffer-xref-test-an-answer-in-a-source-lands-in-the-pane ()
  "An answer pointing into a source buffer is moved to the pane showing it."
  (align-buffer-xref-tests--with-session
    (let ((align-buffer-xref-tests--answer
           (list (xref-make "second" (xref-make-buffer-location
                                      new (with-current-buffer new
                                            (goto-char (point-min))
                                            (forward-line 1)
                                            (+ (point) 4)))))))
      (align-buffer-xref-tests--goto session 'right 0 4)
      (let* ((items (xref-backend-definitions 'align-buffer-xref "x"))
             (location (xref-item-location (car items))))
        (should (eq (xref-buffer-location-buffer location)
                    (align-buffer-buffer session 'right)))
        ;; Line two of the new buffer is row one, and the column is kept.
        (should (= (xref-buffer-location-position location)
                   (+ (align-buffer-row-beginning-position session 1 'right)
                      4)))))))

(ert-deftest align-buffer-xref-test-remapping-can-be-turned-off ()
  "With remapping off an answer keeps pointing at the source buffer."
  (align-buffer-xref-tests--with-session
    (let ((align-buffer-xref-remap-results nil)
          (align-buffer-xref-tests--answer
           (list (xref-make "second" (xref-make-buffer-location new 20)))))
      (align-buffer-xref-tests--goto session 'right 0 4)
      (let ((location (xref-item-location
                       (car (xref-backend-definitions 'align-buffer-xref "x")))))
        (should (eq (xref-buffer-location-buffer location) new))))))

(ert-deftest align-buffer-xref-test-an-answer-elsewhere-is-left-alone ()
  "An answer in a buffer no pane shows is not touched."
  (align-buffer-xref-tests--with-session
    (let ((elsewhere (generate-new-buffer " *xref elsewhere*")))
      (unwind-protect
          (let ((align-buffer-xref-tests--answer
                 (list (xref-make "far" (xref-make-buffer-location elsewhere 1)))))
            (align-buffer-xref-tests--goto session 'right 0 4)
            (let ((location (xref-item-location
                             (car (xref-backend-definitions
                                   'align-buffer-xref "x")))))
              (should (eq (xref-buffer-location-buffer location) elsewhere))))
        (kill-buffer elsewhere)))))

(cl-defstruct (align-buffer-xref-tests-location
               (:constructor align-buffer-xref-tests-location-new
                             (file position)))
  "A location class of its own, as `xref-elisp-location' is."
  file position)

(cl-defmethod xref-location-group ((location align-buffer-xref-tests-location))
  (align-buffer-xref-tests-location-file location))

(cl-defmethod xref-location-marker ((location align-buffer-xref-tests-location))
  (with-current-buffer (find-file-noselect
                        (align-buffer-xref-tests-location-file location))
    (copy-marker (align-buffer-xref-tests-location-position location))))

(ert-deftest align-buffer-xref-test-another-backends-location-class ()
  "A location neither file nor buffer is read through the generic accessors.
The elisp backend answers with `xref-elisp-location', which knows its file only
through `xref-location-group' and its position only through a marker."
  (let* ((file (make-temp-file "align-buffer-xref-" nil ".txt"
                               "int first(int a);
int second(int b);
"))
         (source (find-file-noselect file)))
    (unwind-protect
        (let ((session
               (align-buffer-show
                (align-buffer-plan-create
                 :left-name " *class left*"
                 :right-name " *class right*"
                 :rows (vector
                        (align-buffer-row-create
                         :left (align-buffer-cell-create :kind 'blank)
                         :right (align-buffer-cell-create
                                 :kind 'line :source source :line 1))
                        (align-buffer-row-create
                         :left (align-buffer-cell-create :kind 'blank)
                         :right (align-buffer-cell-create
                                 :kind 'line :source source :line 2)))))))
          (unwind-protect
              (let* ((position (with-current-buffer source
                                 (save-excursion
                                   (goto-char (point-min))
                                   (forward-line 1)
                                   (+ (point) 4))))
                     (item (xref-make "second"
                                      (align-buffer-xref-tests-location-new
                                       file position)))
                     (remapped (car (align-buffer-xref--remap (list item))))
                     (location (xref-item-location remapped)))
                (should (eq (xref-buffer-location-buffer location)
                            (align-buffer-buffer session 'right)))
                (should (= (xref-buffer-location-position location)
                           (+ (align-buffer-row-beginning-position session 1 'right)
                              4))))
            (align-buffer-quit session)))
      (kill-buffer source)
      (delete-file file))))

(ert-deftest align-buffer-xref-test-the-backend-survives-a-rebuild ()
  "Rendering clears buffer-local hooks, so the panes get the backend again."
  (align-buffer-xref-tests--with-session
    (align-buffer-rebuild session (align-buffer-session-plan session))
    (align-buffer-xref-tests--goto session 'left 0 4)
    (should (eq (xref-find-backend) 'align-buffer-xref))))

(provide 'align-buffer-xref-tests)

;;; align-buffer-xref-tests.el ends here
