;;; align-diff-vc.el --- Diff a file against a revision of itself -*- lexical-binding: t; -*-

;; Author: Thomas Ferrand
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, vc
;; URL: https://github.com/aTom3333/align-buffer

;;; Commentary:

;; Entry points that read a revision into a buffer and hand it to
;; `align-diff-buffers': `align-diff-revision' compares a file's current text
;; against a revision of it, `align-diff-revisions' compares two revisions.
;;
;; Everything goes through vc, so any backend vc handles works, and the revision
;; buffer belongs to the session: it is named on the plan and killed with it.

;;; Code:

(require 'align-diff)
(require 'vc)

(defun align-diff-vc--backend (file)
  "Return FILE's version control backend, or signal."
  (or (vc-backend file)
      (user-error "%s is not under version control"
                  (abbreviate-file-name file))))

;;;###autoload
(defun align-diff-revision-buffer (file revision &optional absent-ok)
  "Return a buffer holding FILE as of REVISION, under FILE's own major mode.
With ABSENT-OK, a revision without that file gives an empty buffer."
  (let ((backend (align-diff-vc--backend file))
        (buffer (generate-new-buffer
                 (format "%s@%s" (file-name-nondirectory file) revision))))
    ;; vc runs the backend command in this buffer's directory, and a new buffer
    ;; inherits the caller's.
    (with-current-buffer buffer
      (setq default-directory (file-name-directory file)))
    (condition-case error
        (progn
          ;; The no-save reader: `vc-find-revision' writes FILE.~REVISION~
          ;; into the working tree.
          (vc-find-revision-no-save file revision backend buffer)
          ;; vc sets the mode with the hooks delayed, so nothing turned
          ;; font-lock on, and the panes copy the faces it computes.
          (with-current-buffer buffer (font-lock-mode 1))
          buffer)
      (error
       (if absent-ok
           ;; The failed command left its complaint in the buffer.
           (with-current-buffer buffer
             (let ((inhibit-read-only t))
               (erase-buffer))
             (let ((buffer-file-name file)
                   (enable-local-variables :safe))
               (ignore-errors (delay-mode-hooks (set-auto-mode))))
             (font-lock-mode 1)
             (set-buffer-modified-p nil)
             buffer)
         (kill-buffer buffer)
         (user-error "Cannot read %s at %s: %s"
                     (file-name-nondirectory file) revision
                     (error-message-string error)))))))

;;;###autoload
(defun align-diff-revision (revision &optional file)
  "Show FILE as of REVISION on the left, its current text on the right.
FILE defaults to the current buffer's, which the right pane reads, so unsaved
edits show."
  (interactive
   (list (vc-read-revision "Revision: "
                           (list (or (buffer-file-name)
                                     (user-error "Not visiting a file")))
                           nil "HEAD")))
  (let* ((new (if file
                  (or (find-buffer-visiting file) (find-file-noselect file))
                (current-buffer)))
         (path (or file (buffer-file-name new)
                   (user-error "Not visiting a file")))
         (old (align-diff-revision-buffer path revision)))
    (align-diff-buffers old new (list :owned-buffers (list old)))))

;;;###autoload
(defun align-diff-revisions (old-revision new-revision &optional file)
  "Show FILE as of OLD-REVISION and as of NEW-REVISION, side by side."
  (interactive
   (let ((path (or (buffer-file-name) (user-error "Not visiting a file"))))
     (list (vc-read-revision "Old revision: " (list path) nil "HEAD")
           (vc-read-revision "New revision: " (list path)))))
  (let* ((path (or file (buffer-file-name) (user-error "Not visiting a file")))
         (old (align-diff-revision-buffer path old-revision))
         (new (align-diff-revision-buffer path new-revision)))
    (align-diff-buffers old new (list :owned-buffers (list old new)))))

(provide 'align-diff-vc)

;;; align-diff-vc.el ends here
