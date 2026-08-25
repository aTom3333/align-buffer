;;; align-buffer-eglot.el --- A server's semantic faces in the panes -*- lexical-binding: t; -*-

;; Author: Thomas Ferrand
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, tools
;; URL: https://github.com/aTom3333/align-buffer

;;; Commentary:

;; eglot paints a server's semantic tokens as `face' text properties, through a
;; font-lock keyword, and a pane copies `face' from the lines it reads - so the
;; colours arrive on their own whenever the source already has them.
;;
;; A source nobody has displayed does not have them: eglot's keyword asks the
;; server and returns nothing, and when the answer lands nothing refontifies a
;; buffer that is not on screen.  So this waits for the tokens to arrive, makes
;; the source fontify, and copies its faces into the panes again.

;;; Code:

(require 'align-buffer)

(defgroup align-buffer-eglot nil
  "Semantic faces from a language server, in align-buffer's panes."
  :group 'align-buffer
  :prefix "align-buffer-eglot-")

(defcustom align-buffer-eglot-attempts 60
  "How many times to look for a source's semantic tokens before giving up.
A cold server can take a while to parse a project."
  :type 'natnum
  :group 'align-buffer-eglot)

(defcustom align-buffer-eglot-interval 1
  "Seconds between two looks for a source's semantic tokens."
  :type 'number
  :group 'align-buffer-eglot)

(defun align-buffer-eglot--painting-p (buffer)
  "Return non-nil when BUFFER is one eglot paints semantic tokens in."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (bound-and-true-p eglot--managed-mode)
              (bound-and-true-p eglot-semantic-tokens-mode)))))

(defun align-buffer-eglot--tokens-ready-p (buffer)
  "Return non-nil when BUFFER has tokens for the text it holds now.
The same three things eglot's own font-lock keyword asks: tokens for this
document version, the request dispatched, and data to show."
  (with-current-buffer buffer
    (let ((state (and (boundp 'eglot--semtok-state) eglot--semtok-state)))
      (and state
           (eql (plist-get state :docver) (bound-and-true-p eglot--docver))
           (plist-get state :dispatched)
           (> (length (plist-get state :data)) 0)))))

(defun align-buffer-eglot--fontify (buffer)
  "Fontify BUFFER again, which is what puts the tokens on its text.
Also what asks for them: eglot requests from the font-lock keyword."
  (with-current-buffer buffer
    (font-lock-flush)
    (font-lock-ensure)))

(defun align-buffer-eglot-refresh (buffer)
  "Copy BUFFER's semantic faces into every pane that reads it."
  (when (align-buffer-eglot--painting-p buffer)
    (align-buffer-eglot--fontify buffer)
    (dolist (session (align-buffer-sessions-for-source buffer))
      (align-buffer-refresh-faces session buffer))))

(defun align-buffer-eglot--follow (buffer attempts)
  "Copy BUFFER's semantic faces once they exist, looking ATTEMPTS times."
  (when (and (align-buffer-eglot--painting-p buffer)
             ;; Nothing shows this buffer any more; nothing to paint.
             (align-buffer-sessions-for-source buffer))
    (if (align-buffer-eglot--tokens-ready-p buffer)
        (align-buffer-eglot-refresh buffer)
      (when (> attempts 0)
        ;; The first look also asks, since a source that was fontified before
        ;; the server was ready will otherwise never request anything.
        (when (= attempts align-buffer-eglot-attempts)
          (align-buffer-eglot--fontify buffer))
        (run-at-time align-buffer-eglot-interval nil
                     #'align-buffer-eglot--follow buffer (1- attempts))))))

(defun align-buffer-eglot--post-build (session)
  "Follow the semantic tokens of every source SESSION reads."
  (dolist (buffer (align-buffer-sources session))
    (align-buffer-eglot--follow buffer align-buffer-eglot-attempts)))

(add-hook 'align-buffer-post-build-functions #'align-buffer-eglot--post-build)

(defun align-buffer-eglot--server-refresh (_server method &rest _)
  "Follow the tokens again when a server says METHOD is a refresh request."
  (when (eq method 'workspace/semanticTokens/refresh)
    (dolist (session (align-buffer-sessions))
      (dolist (buffer (align-buffer-sources session))
        (align-buffer-eglot--follow buffer align-buffer-eglot-attempts)))))

(with-eval-after-load 'eglot
  (advice-add 'eglot-handle-request :after #'align-buffer-eglot--server-refresh))

(provide 'align-buffer-eglot)

;;; align-buffer-eglot.el ends here
