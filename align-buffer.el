;;; align-buffer.el --- Scroll two buffers to each other by a line correspondence -*- lexical-binding: t; -*-

;; Author: Thomas Ferrand
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, files
;; URL: https://github.com/aTom3333/align-buffer

;;; Commentary:

;; align-buffer shows two buffers side by side and keeps them scrolled to each
;; other, so that lines which correspond sit level.  The buffers are the REAL
;; ones: their major mode, their font lock, their language server, their
;; bindings and their editing all work, because nothing is copied anywhere.
;;
;; What corresponds is given, not worked out here: a set of
;; (LEFT-LINE . RIGHT-LINE) pairs.  Where those come from is somebody else's
;; business.  The correspondence may be partial, many to one, and in any order,
;; so a block that moved can pair its old lines with new ones elsewhere in the
;; file and the two windows will jump to each other across it.
;;
;; Exact alignment of every line is not on offer and cannot be: Emacs shows a
;; buffer's own lines, in its own order, and a line with no counterpart has
;; nowhere to go.  What is on offer is that the pair governing the line point is
;; on is always level, which is the line being read.
;;
;; A consumer that wants more than that hangs a pad where the lines are
;; missing, with `align-buffer-pad-add': a blank row for each of them, which
;; leaves every line facing either its own counterpart or a row standing in for
;; it.  The rows are walked and scrolled through like lines of their own.
;;
;; This file owns the lifecycle: set up, change the correspondence, tear down,
;; and the hooks a consumer uses to put the windows where it wants them.

;;; Code:

(require 'align-buffer-core)
(require 'align-buffer-sync)
(require 'align-buffer-pads)

(defcustom align-buffer-layout-function #'align-buffer-layout-side-by-side
  "Function laying out a session's two buffers.
Called with the session and the window to build in, and returns the windows it
put the buffers in, as a list."
  :type 'function
  :group 'align-buffer)

(defvar align-buffer-post-layout-functions nil
  "Functions run with the session and the windows showing its buffers.")

(defvar align-buffer-pre-teardown-functions nil
  "Functions run with the session before it is taken apart.")

(defvar align-buffer--tearing-down nil
  "Non-nil while we are taking a session apart.")


;;; The mode carrying a session

;;;###autoload
(define-minor-mode align-buffer-mode
  "Minor mode of a buffer taking part in an align-buffer session.
Carries the sync and the session this buffer belongs to.  Set up by
`align-buffer-show'; turning it off leaves the buffer as it was."
  :lighter " Align"
  :group 'align-buffer
  (cond
   (align-buffer-mode
    (align-buffer-sync-enable)
    (add-hook 'kill-buffer-hook #'align-buffer--buffer-killed nil t))
   (t
    (align-buffer-sync-disable)
    (remove-hook 'kill-buffer-hook #'align-buffer--buffer-killed t)
    (kill-local-variable 'align-buffer--session)
    (kill-local-variable 'align-buffer--side)
    (kill-local-variable 'align-buffer--line-cache))))


;;; Layout

(defun align-buffer-layout-side-by-side (session window)
  "Show SESSION's buffers side by side in WINDOW, and return the two windows."
  (let* ((window (or (and (window-live-p window) window) (selected-window)))
         (left window)
         (right (split-window window nil 'right)))
    (set-window-buffer left (align-buffer-session-left-buffer session))
    (set-window-buffer right (align-buffer-session-right-buffer session))
    (list left right)))


;;; Setting up

(defun align-buffer--claim (session side)
  "Let SESSION's buffer for SIDE take part, and remember where point was."
  (with-current-buffer (align-buffer-buffer session side)
    (if (eq side 'left)
        (setf (align-buffer-session-left-point session) (point))
      (setf (align-buffer-session-right-point session) (point)))
    (setq align-buffer--session session)
    (setq align-buffer--side side)
    (align-buffer-mode 1)))

;;;###autoload
(defun align-buffer-show (left right pairs &optional properties window)
  "Show buffers LEFT and RIGHT side by side, scrolled to each other by PAIRS.

PAIRS is a list or vector of (LEFT-LINE . RIGHT-LINE) conses of one-based line
numbers, saying which line of LEFT stands level with which line of RIGHT.  It
need not be sorted, total, one to one, or monotonic.

PROPERTIES is a plist carried on the session for the consumer's own use.
WINDOW is the window to lay the two out in, defaulting to the selected one.

Return the session.  Signals when either buffer already belongs to one, since a
buffer can only be scrolled to one other at a time."
  (unless (and (buffer-live-p (get-buffer left)) (buffer-live-p (get-buffer right)))
    (error "Both sides must be live buffers"))
  (let ((left (get-buffer left))
        (right (get-buffer right)))
    (when (eq left right)
      (error "A buffer cannot be shown against itself"))
    (dolist (buffer (list left right))
      (when (align-buffer-session-for buffer)
        (user-error "%s already belongs to a session" (buffer-name buffer))))
    (align-buffer-check-pairs pairs)
    (let ((session (align-buffer-session-create
                    :left-buffer left
                    :right-buffer right
                    :left-index (align-buffer-build-index pairs 'left)
                    :right-index (align-buffer-build-index pairs 'right)
                    :properties properties)))
      (push session align-buffer--sessions)
      (dolist (side '(left right))
        (align-buffer--claim session side))
      (let ((windows (funcall align-buffer-layout-function session window)))
        (setf (align-buffer-session-windows session) windows)
        (run-hook-with-args 'align-buffer-post-layout-functions session windows)
        (when-let ((last-window (car (last windows))))
          (when (window-live-p last-window) (select-window last-window)))
        (align-buffer-sync (selected-window))
        session))))

(defun align-buffer-set-pairs (session pairs)
  "Replace SESSION's correspondence with PAIRS and bring the windows level again."
  (align-buffer-check-pairs pairs)
  (setf (align-buffer-session-left-index session)
        (align-buffer-build-index pairs 'left))
  (setf (align-buffer-session-right-index session)
        (align-buffer-build-index pairs 'right))
  (dolist (side '(left right))
    (let ((window (get-buffer-window (align-buffer-buffer session side))))
      (when window (align-buffer-forget-placement window))))
  (let ((window (selected-window)))
    (when (align-buffer-session-for (window-buffer window))
      (align-buffer-sync window)))
  session)


;;; Taking it apart

(defun align-buffer-quit (&optional session)
  "Take SESSION apart, defaulting to the one of the current buffer.
The two buffers are left alive and as they were; point goes back to where it
was in whichever of them the reader is not standing in."
  (interactive)
  (let ((session (or session (align-buffer-current-session))))
    (when (and session (not align-buffer--tearing-down))
      (let ((align-buffer--tearing-down t)
            (here (window-buffer (selected-window))))
        (run-hook-with-args 'align-buffer-pre-teardown-functions session)
        (setq align-buffer--sessions (delq session align-buffer--sessions))
        (align-buffer-forget-pending-place session)
        ;; After the hook, because it can delete windows on its own.
        (let ((windows (mapcan (lambda (side)
                                 (get-buffer-window-list
                                  (align-buffer-buffer session side) nil t))
                               '(left right))))
          (mapc #'align-buffer-forget-placement windows)
          ;; A window left lifted would show its first line part way off.
          (mapc #'align-buffer-forget-vscroll windows))
        (dolist (side '(left right))
          (let ((buffer (align-buffer-buffer session side)))
            (when (buffer-live-p buffer)
              (with-current-buffer buffer (align-buffer-mode -1)))))
        (dolist (window (cdr (align-buffer-session-windows session)))
          (when (and (window-live-p window)
                     (eq (window-deletable-p window) t))
            (delete-window window)))
        ;; After the windows, because selecting one copies its own point over
        ;; the buffer's, and deleting a window selects another.
        (dolist (side '(left right))
          (align-buffer--restore-point session side here))
        (align-buffer--kill-owned session)))))

(defun align-buffer--restore-point (session side here)
  "Put point in SESSION's SIDE buffer back where it was, unless it is HERE.
The sync moves the point of a WINDOW, so every window still showing the buffer
is given it back as well."
  (let ((buffer (align-buffer-buffer session side))
        (point (if (eq side 'left)
                   (align-buffer-session-left-point session)
                 (align-buffer-session-right-point session))))
    (when (and point (buffer-live-p buffer) (not (eq buffer here)))
      (with-current-buffer buffer
        (let ((position (min point (point-max))))
          (goto-char position)
          (dolist (window (get-buffer-window-list buffer nil t))
            (set-window-point window position)))))))

(defun align-buffer--kill-owned (session)
  "Kill the buffers SESSION's properties named as its own.
A buffer the reader has since edited is theirs now, whoever made it."
  (dolist (buffer (plist-get (align-buffer-session-properties session)
                             :owned-buffers))
    (when (and (buffer-live-p buffer) (not (buffer-modified-p buffer)))
      (kill-buffer buffer))))

(defun align-buffer--buffer-killed ()
  "Take the session apart when one of its buffers is killed out of band."
  (align-buffer-quit align-buffer--session))

(provide 'align-buffer)

;;; align-buffer.el ends here
