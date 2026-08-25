;;; align-buffer-eglot-tests.el --- Tests for align-buffer-eglot -*- lexical-binding: t; -*-

;;; Commentary:

;; No server: the source buffers carry the buffer-locals eglot would have set,
;; so these test what this extension decides and copies, not what eglot does.
;;
;; Run with:
;;   emacs -Q --batch -L . -L extensions -l test/align-buffer-eglot-tests.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'align-buffer-eglot)

(defvar align-buffer-eglot-tests--fontified nil
  "The buffers the fake fontification was asked to paint.")

(defun align-buffer-eglot-tests--fontify (buffer)
  "Stand in for eglot's font-lock keyword: paint what the tokens say."
  (push buffer align-buffer-eglot-tests--fontified)
  (with-current-buffer buffer
    (when (align-buffer-eglot--tokens-ready-p buffer)
      (put-text-property (point-min) (+ (point-min) 3) 'face
                         'eglot-semantic-function))))

(defmacro align-buffer-eglot-tests--with-source (state &rest body)
  "Run BODY with `source' managed by a pretend eglot, its semtok STATE given.
`session' shows the source's first two lines on the left."
  (declare (indent 1))
  `(let ((source (generate-new-buffer " *eglot test source*"))
         (align-buffer-eglot-tests--fontified nil))
     (unwind-protect
         (progn
           (with-current-buffer source
             (insert "one one one\ntwo two two\n")
             (setq-local eglot--managed-mode t)
             (setq-local eglot-semantic-tokens-mode t)
             (setq-local eglot--docver 0)
             (setq-local eglot--semtok-state ,state))
           (let ((session
                  (align-buffer-show
                   (align-buffer-plan-create
                    :left-name " *eglot left*"
                    :right-name " *eglot right*"
                    :rows (vector
                           (align-buffer-row-create
                            :left (align-buffer-cell-create
                                   :kind 'line :source source :line 1)
                            :right (align-buffer-cell-create :kind 'blank))
                           (align-buffer-row-create
                            :left (align-buffer-cell-create
                                   :kind 'line :source source :line 2)
                            :right (align-buffer-cell-create :kind 'blank)))))))
             (unwind-protect (progn ,@body)
               (align-buffer-quit session))))
       (kill-buffer source))))

(defun align-buffer-eglot-tests--pane-face (session)
  (with-current-buffer (align-buffer-buffer session 'left)
    (get-text-property (point-min) 'face)))

(ert-deftest align-buffer-eglot-test-tokens-ready ()
  "Ready means tokens for this document version, dispatched, with data."
  (align-buffer-eglot-tests--with-source
      '(:docver 0 :dispatched t :data [1 2 3])
    (should (align-buffer-eglot--tokens-ready-p source)))
  (align-buffer-eglot-tests--with-source '(:docver 0 :dispatched t :data [])
    (should-not (align-buffer-eglot--tokens-ready-p source)))
  (align-buffer-eglot-tests--with-source
      '(:docver 3 :dispatched t :data [1 2 3])
    (should-not (align-buffer-eglot--tokens-ready-p source)))
  (align-buffer-eglot-tests--with-source
      '(:docver 0 :dispatched nil :data [1 2 3])
    (should-not (align-buffer-eglot--tokens-ready-p source)))
  (align-buffer-eglot-tests--with-source nil
    (should-not (align-buffer-eglot--tokens-ready-p source))))

(ert-deftest align-buffer-eglot-test-a-buffer-no-server-paints ()
  "A source eglot does not paint is left alone entirely."
  (let ((source (generate-new-buffer " *plain source*")))
    (unwind-protect
        (should-not (align-buffer-eglot--painting-p source))
      (kill-buffer source))))

(ert-deftest align-buffer-eglot-test-the-build-copies-what-is-there ()
  "Tokens the source already has are in the panes as soon as they are built.
The build hook follows every source, and finds these ready at the first look."
  (cl-letf (((symbol-function 'align-buffer-eglot--fontify)
             #'align-buffer-eglot-tests--fontify))
    (align-buffer-eglot-tests--with-source
        '(:docver 0 :dispatched t :data [1 2 3])
      (should (eq (align-buffer-eglot-tests--pane-face session)
                  'eglot-semantic-function)))))

(ert-deftest align-buffer-eglot-test-tokens-arriving-later ()
  "Tokens that land after the build reach the panes when they do."
  (cl-letf (((symbol-function 'align-buffer-eglot--fontify)
             #'align-buffer-eglot-tests--fontify))
    (align-buffer-eglot-tests--with-source '(:docver 0 :dispatched t :data [])
      (should (null (align-buffer-eglot-tests--pane-face session)))
      (with-current-buffer source
        (setq-local eglot--semtok-state '(:docver 0 :dispatched t :data [1 2 3])))
      (align-buffer-eglot--follow source align-buffer-eglot-attempts)
      (should (eq (align-buffer-eglot-tests--pane-face session)
                  'eglot-semantic-function)))))

(ert-deftest align-buffer-eglot-test-following-waits-for-the-tokens ()
  "With no tokens yet nothing is copied, and another look is scheduled."
  (cl-letf (((symbol-function 'align-buffer-eglot--fontify)
             #'align-buffer-eglot-tests--fontify))
    (align-buffer-eglot-tests--with-source '(:docver 0 :dispatched t :data [])
      (align-buffer-eglot--follow source align-buffer-eglot-attempts)
      (should (null (align-buffer-eglot-tests--pane-face session)))
      ;; The first look asks, since a source fontified before the server was
      ;; ready would otherwise never request anything.
      (should (memq source align-buffer-eglot-tests--fontified))
      (let ((pending (seq-filter
                      (lambda (timer)
                        (eq (timer--function timer)
                            #'align-buffer-eglot--follow))
                      timer-list)))
        (should pending)
        (dolist (timer pending) (cancel-timer timer))))))

(ert-deftest align-buffer-eglot-test-following-stops-with-the-session ()
  "Nothing is scheduled for a source no pane reads any more."
  (let ((source (generate-new-buffer " *unwatched source*")))
    (unwind-protect
        (progn
          (with-current-buffer source
            (insert "one\n")
            (setq-local eglot--managed-mode t)
            (setq-local eglot-semantic-tokens-mode t)
            (setq-local eglot--docver 0)
            (setq-local eglot--semtok-state '(:docver 0 :dispatched t :data [])))
          (align-buffer-eglot--follow source align-buffer-eglot-attempts)
          (should-not (seq-filter (lambda (timer)
                                    (eq (timer--function timer)
                                        #'align-buffer-eglot--follow))
                                  timer-list)))
      (kill-buffer source))))

(provide 'align-buffer-eglot-tests)

;;; align-buffer-eglot-tests.el ends here
