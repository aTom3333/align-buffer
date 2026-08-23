;;; align-buffer.el --- Show two buffers side by side with aligned lines -*- lexical-binding: t; -*-

;; Author: Thomas Ferrand
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, files
;; URL: https://github.com/aTom3333/align-buffer

;;; Commentary:

;; align-buffer shows two buffers side by side with their lines vertically
;; ALIGNED according to a plan, keeps the two views scroll-synced, and navigates
;; the rows the plan marks as interesting.  A plan says which line of which
;; buffer sits on which row; where a plan comes from is somebody else's business.
;;
;; The two buffers it builds are its PANES, and that is the word used throughout.
;; They are synthetic and read-only, holding one real line per row, so padding is
;; a real empty line rather than an overlay string: real lines scroll through,
;; take overlays, and answer to point like any other text.
;;
;; This file owns the lifecycle: build, rebuild, tear down, and the hooks a
;; consumer uses to put the panes where it wants them.

;;; Code:

(require 'align-buffer-core)
(require 'align-buffer-render)
(require 'align-buffer-sync)
(require 'align-buffer-navigation)

(defcustom align-buffer-layout-function #'align-buffer-layout-side-by-side
  "Function laying out a session's panes.
Called with the session and the window to build in, and returns the windows it
put the panes in, as a list."
  :type 'function
  :group 'align-buffer)

(defvar align-buffer-post-build-functions nil
  "Functions run with the session once its panes are built.
Before any window shows them, for a consumer that wants the maps and nothing on
screen.")

(defvar align-buffer-post-layout-functions nil
  "Functions run with the session and the windows showing its panes.")

(defvar align-buffer-pre-teardown-functions nil
  "Functions run with the session before its panes are killed.")

(defvar align-buffer--tearing-down nil
  "Non-nil while we are tearing a session down.")

(defvar align-buffer--killing nil
  "The pane buffer being killed, so teardown does not kill it again.")


;;; Layout

(defun align-buffer-layout-side-by-side (session window)
  "Show SESSION's panes side by side in WINDOW, and return the two windows."
  (let* ((window (or (and (window-live-p window) window) (selected-window)))
         (left window)
         (right (split-window window nil 'right)))
    (set-window-buffer left (align-buffer-session-left-buffer session))
    (set-window-buffer right (align-buffer-session-right-buffer session))
    (list left right)))


;;; Building a session

(defun align-buffer--pane-buffer (name fallback)
  "Return a new pane buffer, named NAME or after FALLBACK when NAME is nil."
  (generate-new-buffer (or name (format "*%s*" fallback))))

(defun align-buffer--make-session (plan)
  "Return a session for PLAN, with its rows normalised and its panes created."
  (let ((rows (align-buffer-plan-rows plan)))
    (setf (align-buffer-plan-rows plan)
          (if (vectorp rows) rows (vconcat rows))))
  (let ((session
         (align-buffer-session-create
          :plan plan
          :left-buffer (align-buffer--pane-buffer
                        (align-buffer-plan-left-name plan) "align-buffer left")
          :right-buffer (align-buffer--pane-buffer
                         (align-buffer-plan-right-name plan) "align-buffer right"))))
    (dolist (side '(left right))
      (with-current-buffer (align-buffer-buffer session side)
        (add-hook 'kill-buffer-hook #'align-buffer--pane-killed nil t)))
    session))


;;; Lifecycle

;;;###autoload
(defun align-buffer-show (plan &optional window)
  "Build PLAN's panes, lay them out in WINDOW, and return the session."
  (let ((session (align-buffer--make-session plan)))
    (condition-case error
        (align-buffer-render session)
      (error (dolist (side '(left right))
               (let ((buffer (align-buffer-buffer session side)))
                 (when (buffer-live-p buffer)
                   (let ((align-buffer--tearing-down t))
                     (kill-buffer buffer)))))
             (signal (car error) (cdr error))))
    (push session align-buffer--sessions)
    (run-hook-with-args 'align-buffer-post-build-functions session)
    (let ((windows (funcall align-buffer-layout-function session window)))
      (run-hook-with-args 'align-buffer-post-layout-functions session windows)
      (when-let ((last-window (car (last windows))))
        (when (window-live-p last-window) (select-window last-window)))
      session)))

(defun align-buffer-rebuild (session plan)
  "Show PLAN in SESSION's existing panes, keeping each one where it is scrolled.
The same buffers and the same windows; only the rows change."
  (let ((tops (mapcar (lambda (side)
                        (let ((window (get-buffer-window
                                       (align-buffer-buffer session side))))
                          (cons side (and window
                                          (align-buffer--viewport-row window)))))
                      '(left right))))
    (setf (align-buffer-session-plan session) plan)
    (setf (align-buffer-plan-rows plan)
          (let ((rows (align-buffer-plan-rows plan)))
            (if (vectorp rows) rows (vconcat rows))))
    (align-buffer-render session)
    (run-hook-with-args 'align-buffer-post-build-functions session)
    (pcase-dolist (`(,side . ,row) tops)
      (let ((window (get-buffer-window (align-buffer-buffer session side))))
        (when (and window row)
          (align-buffer--place session window side row))))
    (run-hook-with-args 'align-buffer-post-layout-functions session
                        (delq nil (mapcar (lambda (side)
                                            (get-buffer-window
                                             (align-buffer-buffer session side)))
                                          '(left right))))
    session))

(defun align-buffer-refresh ()
  "Build the current session's plan again, keeping the panes where they are."
  (interactive)
  (let ((session (or (align-buffer-current-session)
                     (user-error "Not in an align-buffer pane"))))
    (align-buffer-rebuild session (align-buffer-session-plan session))))

(defun align-buffer-quit (&optional session)
  "Tear SESSION down, defaulting to the one of the current pane."
  (interactive)
  (let ((session (or session (align-buffer-current-session))))
    (when (and session (not align-buffer--tearing-down))
      (let ((align-buffer--tearing-down t)
            (windows (mapcan (lambda (side)
                               (get-buffer-window-list
                                (align-buffer-buffer session side) nil t))
                             '(left right))))
        (run-hook-with-args 'align-buffer-pre-teardown-functions session)
        (setq align-buffer--sessions (delq session align-buffer--sessions))
        (align-buffer-forget-pending-place session)
        (mapc #'align-buffer-forget-placement windows)
        (dolist (window (cdr windows))
          (when (window-live-p window) (delete-window window)))
        (dolist (side '(left right))
          (let ((buffer (align-buffer-buffer session side)))
            (when (and (buffer-live-p buffer) (not (eq buffer align-buffer--killing)))
              (kill-buffer buffer))))
        (align-buffer--kill-owned session)))))

(defun align-buffer--kill-owned (session)
  "Kill the buffers SESSION's plan named as its own.
A buffer the reader has since edited is theirs now, whoever made it."
  (dolist (buffer (plist-get (align-buffer-plan-properties
                              (align-buffer-session-plan session))
                             :owned-buffers))
    (when (and (buffer-live-p buffer) (not (buffer-modified-p buffer)))
      (kill-buffer buffer))))

(defun align-buffer--pane-killed ()
  "Tear the session down when one of its panes is killed out of band."
  (let ((align-buffer--killing (current-buffer)))
    (align-buffer-quit align-buffer--session)))

(provide 'align-buffer)

;;; align-buffer.el ends here
