;;; align-diff-vc-tests.el --- Tests for align-diff-vc -*- lexical-binding: t; -*-

;;; Commentary:

;; These drive a real repository, made under `align-diff-vc-tests-directory' and
;; deleted again, so they need git on PATH.
;;
;; The fixture directory is an ordinary variable, never `default-directory':
;; that one is buffer-local, so a `let' around code that changes buffers holds
;; only in the buffer it was made in, and a recursive delete reading it
;; afterwards deletes the directory of whatever buffer is current.
;;
;; Run with:
;;   emacs -Q --batch -L . -L extensions -l test/align-diff-vc-tests.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'align-diff-vc)

(defvar align-diff-vc-tests-directory "d:/temp/align-diff-vc-tests/"
  "Where the test repository is made.  Never the C: drive, never a source tree.")

(defun align-diff-vc-tests--scratch-p (directory)
  "Return non-nil when DIRECTORY is one this file is allowed to delete."
  (let ((expanded (expand-file-name directory)))
    (and (string-prefix-p (expand-file-name "d:/temp/") expanded)
         (string-match-p "align-diff-vc-tests" expanded))))

(defun align-diff-vc-tests--remove (directory)
  "Delete DIRECTORY, refusing anything but this file's own scratch directory."
  (unless (align-diff-vc-tests--scratch-p directory)
    (error "Refusing to delete %s" directory))
  (when (file-directory-p directory)
    (delete-directory directory t)))

(defun align-diff-vc-tests--git (directory &rest arguments)
  (let* ((default-directory directory)
         (status (apply #'call-process "git" nil nil nil arguments)))
    (unless (zerop status)
      (error "git %s failed with %s" (string-join arguments " ") status))))

(defun align-diff-vc-tests--write (file text)
  (with-temp-file file (insert text)))

(defun align-diff-vc-tests--forget (directory)
  "Kill the buffers visiting files under DIRECTORY."
  (dolist (buffer (buffer-list))
    (let ((file (buffer-file-name buffer)))
      (when (and file (string-prefix-p (expand-file-name directory)
                                       (expand-file-name file)))
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer)))))

(defmacro align-diff-vc-tests--with-repository (&rest body)
  "Run BODY in a repository with `sample' committed twice and then edited.
`sample' is a file's name, `added' one staged but never committed, and
`repository' the directory holding them."
  (declare (indent 0))
  `(let* ((repository (file-name-as-directory align-diff-vc-tests-directory))
          (sample (expand-file-name "sample.txt" repository))
          (added (expand-file-name "added.txt" repository)))
     (align-diff-vc-tests--remove repository)
     (make-directory repository t)
     (unwind-protect
         (progn
           (align-diff-vc-tests--git repository "init" "-q" ".")
           (align-diff-vc-tests--git repository "config" "user.email" "t@example.com")
           (align-diff-vc-tests--git repository "config" "user.name" "Test")
           (align-diff-vc-tests--write sample "one\ntwo\nthree\n")
           (align-diff-vc-tests--git repository "add" "sample.txt")
           (align-diff-vc-tests--git repository "commit" "-q" "-m" "first")
           (align-diff-vc-tests--write sample "one\ntwo changed\nthree\n")
           (align-diff-vc-tests--git repository "commit" "-q" "-a" "-m" "second")
           ;; The working tree now differs from both commits.
           (align-diff-vc-tests--write sample "one\ntwo changed\nthree\nfour\n")
           (align-diff-vc-tests--write added "brand new\n")
           (align-diff-vc-tests--git repository "add" "added.txt")
           ,@body)
       (align-diff-vc-tests--forget repository)
       (align-diff-vc-tests--remove repository))))

(defun align-diff-vc-tests--pane (session side)
  (with-current-buffer (align-buffer-buffer session side)
    (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest align-diff-vc-test-refuses-to-delete-anything-else ()
  "The fixture's own delete refuses a directory that is not its scratch one."
  (should-error (align-diff-vc-tests--remove "d:/sources/align-buffer/"))
  (should-error (align-diff-vc-tests--remove default-directory))
  (should-error (align-diff-vc-tests--remove "d:/temp/")))

(ert-deftest align-diff-vc-test-a-file-against-a-revision ()
  "The revision is on the left, the buffer's own text on the right."
  (align-diff-vc-tests--with-repository
    (with-current-buffer (find-file-noselect sample)
      (let ((session (align-diff-revision "HEAD")))
        (unwind-protect
            (progn
              (should (= (align-buffer-row-count session) 4))
              (should (equal (align-diff-vc-tests--pane session 'left)
                             "one\ntwo changed\nthree\n\n"))
              (should (equal (align-diff-vc-tests--pane session 'right)
                             "one\ntwo changed\nthree\nfour\n")))
          (align-buffer-quit session))))))

(ert-deftest align-diff-vc-test-unsaved-edits-are-what-the-right-pane-shows ()
  "The right pane reads the buffer, so it shows text that is not on disk yet."
  (align-diff-vc-tests--with-repository
    (with-current-buffer (find-file-noselect sample)
      (goto-char (point-max))
      (insert "five\n")
      (let ((session (align-diff-revision "HEAD")))
        (unwind-protect
            (should (string-suffix-p "four\nfive\n"
                                     (align-diff-vc-tests--pane session 'right)))
          (align-buffer-quit session)))
      (set-buffer-modified-p nil))))

(ert-deftest align-diff-vc-test-the-revision-buffer-goes-with-the-session ()
  "The buffer holding the revision is owned, and nothing is written to the tree."
  (align-diff-vc-tests--with-repository
    (with-current-buffer (find-file-noselect sample)
      (let* ((session (align-diff-revision "HEAD"))
             (owned (plist-get (align-buffer-plan-properties
                                (align-buffer-session-plan session))
                               :owned-buffers)))
        (should (= (length owned) 1))
        (should (buffer-live-p (car owned)))
        (align-buffer-quit session)
        (should-not (buffer-live-p (car owned)))
        ;; `vc-find-revision' would have left sample.txt.~HEAD~ behind.
        (should (equal (directory-files repository nil "sample")
                       '("sample.txt")))))))

(ert-deftest align-diff-vc-test-two-revisions ()
  "Two revisions of one file, neither of them the working tree."
  (align-diff-vc-tests--with-repository
    (let ((session (align-diff-revisions "HEAD~1" "HEAD" sample)))
      (unwind-protect
          (progn
            (should (equal (align-diff-vc-tests--pane session 'left)
                           "one\ntwo\nthree\n"))
            (should (equal (align-diff-vc-tests--pane session 'right)
                           "one\ntwo changed\nthree\n"))
            (should (= (length (plist-get (align-buffer-plan-properties
                                           (align-buffer-session-plan session))
                                          :owned-buffers))
                       2)))
        (align-buffer-quit session)))))

(ert-deftest align-diff-vc-test-a-file-the-revision-does-not-have ()
  "A file added since the revision reads as empty, but only when asked."
  (align-diff-vc-tests--with-repository
    (should-error (align-diff-revision-buffer added "HEAD") :type 'user-error)
    (let ((buffer (align-diff-revision-buffer added "HEAD" t)))
      (unwind-protect
          (should (= (buffer-size buffer) 0))
        (kill-buffer buffer)))))

(ert-deftest align-diff-vc-test-a-file-not-under-version-control ()
  "A file no backend claims is refused before anything is built."
  (let ((file (make-temp-file "align-diff-vc-loose-" nil ".txt" "text\n")))
    (unwind-protect
        (should-error (align-diff-revision-buffer file "HEAD") :type 'user-error)
      (delete-file file))))

(provide 'align-diff-vc-tests)

;;; align-diff-vc-tests.el ends here
