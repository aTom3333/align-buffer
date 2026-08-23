;;; align-buffer-xref.el --- Answer xref in a pane through its source -*- lexical-binding: t; -*-

;; Author: Thomas Ferrand
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, tools
;; URL: https://github.com/aTom3333/align-buffer

;;; Commentary:

;; A pane's text is a copy, so nothing in it can be looked up: point sits in a
;; buffer no tag table, language server or backend has ever heard of.  This adds
;; an xref backend to the panes that puts every question to the SOURCE buffer's
;; own backend instead, asked at the position the row and the column stand for,
;; and brings the answer back into the pane when it lands on a line a pane shows.
;;
;; Whichever backend the source has is the one that answers, so there is nothing
;; here about diffs and nothing about language servers.  A lookup that reads
;; point itself instead of going through a backend, as eglot's
;; `eglot-find-implementation' does, is not covered: it never goes through a
;; backend, so it needs work of its own.

;;; Code:

(require 'align-buffer)
(require 'seq)
(require 'xref)

(defgroup align-buffer-xref nil
  "Answer xref in a pane through the buffer its rows come from."
  :group 'align-buffer
  :prefix "align-buffer-xref-")

(defcustom align-buffer-xref-remap-results t
  "When non-nil, an answer landing on a line a pane shows goes to that pane."
  :type 'boolean
  :group 'align-buffer-xref)


;;; Asking the source

(defun align-buffer-xref--source-at-point ()
  "Return (BUFFER . POSITION) for the source text point stands on, or nil."
  (let ((session (align-buffer-current-session))
        (side (align-buffer-side)))
    (and session side
         (align-buffer-source-position
          session (align-buffer-row-at-point) side
          (- (point) (line-beginning-position))))))

(defun align-buffer-xref--ask (function &rest arguments)
  "Call FUNCTION with the source's own backend and ARGUMENTS, in the source.
Point is at the position the row and column stand for."
  (let ((source (align-buffer-xref--source-at-point)))
    (when source
      (with-current-buffer (car source)
        (save-excursion
          ;; Point, not the identifier: eglot's backend reads point and ignores
          ;; the identifier it was handed.
          (goto-char (cdr source))
          (let ((backend (xref-find-backend)))
            ;; Our own backend would send the question straight back.
            (when (and backend (not (eq backend 'align-buffer-xref)))
              (apply function backend arguments))))))))


;;; The backend

;;;###autoload
(defun align-buffer-xref-backend ()
  "Return the pane's xref backend when point stands on a source line."
  (and (align-buffer-xref--source-at-point) 'align-buffer-xref))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql align-buffer-xref)))
  (align-buffer-xref--ask #'xref-backend-identifier-at-point))

(cl-defmethod xref-backend-identifier-completion-table
  ((_backend (eql align-buffer-xref)))
  (align-buffer-xref--ask #'xref-backend-identifier-completion-table))

(cl-defmethod xref-backend-definitions ((_backend (eql align-buffer-xref))
                                        identifier)
  (align-buffer-xref--remap
   (align-buffer-xref--ask #'xref-backend-definitions identifier)))

(cl-defmethod xref-backend-references ((_backend (eql align-buffer-xref))
                                       identifier)
  (align-buffer-xref--remap
   (align-buffer-xref--ask #'xref-backend-references identifier)))

(cl-defmethod xref-backend-apropos ((_backend (eql align-buffer-xref)) pattern)
  (align-buffer-xref--remap
   (align-buffer-xref--ask #'xref-backend-apropos pattern)))


;;; Bringing an answer back into a pane

(defun align-buffer-xref--target-at (buffer position)
  "Return (BUFFER LINE COLUMN) for POSITION in BUFFER, or nil."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (save-excursion
          (goto-char position)
          (list buffer
                (line-number-at-pos position t)
                (- position (line-beginning-position))))))))

(defun align-buffer-xref--location-target (location)
  "Return (BUFFER LINE COLUMN) for LOCATION, or nil when that cannot be told."
  (cond
   ((xref-file-location-p location)
    (when-let ((buffer (find-buffer-visiting
                        (xref-file-location-file location))))
      (list buffer
            (xref-file-location-line location)
            (or (xref-file-location-column location) 0))))
   ((xref-buffer-location-p location)
    (align-buffer-xref--target-at (xref-buffer-location-buffer location)
                                  (xref-buffer-location-position location)))
   (t
    ;; A backend's own class, `xref-elisp-location' among them, answers only the
    ;; generic accessors.  The group is the cheap one and names the file; a
    ;; marker is asked for only once that file turns out to be in a buffer,
    ;; since asking otherwise visits every file an answer mentions.
    (when-let* ((group (ignore-errors (xref-location-group location)))
                (visited (and (stringp group) (find-buffer-visiting group)))
                (marker (ignore-errors (xref-location-marker location))))
      (align-buffer-xref--target-at (or (marker-buffer marker) visited)
                                    (marker-position marker))))))

(defun align-buffer-xref--session-for (buffer)
  "Return the session to show BUFFER's lines in, preferring the current pane's."
  (let ((sessions (align-buffer-sessions-for-source buffer))
        (current (align-buffer-current-session)))
    (if (memq current sessions) current (car sessions))))

(defun align-buffer-xref--pane-position (session buffer line column)
  "Return (POSITION . SIDE) where SESSION shows BUFFER's LINE, or nil.
The pane the question came from wins when both show it."
  (let* ((answers (align-buffer-pane-positions session buffer line column))
         (side (align-buffer-side))
         (chosen (or (seq-find (lambda (answer) (eq (cddr answer) side)) answers)
                     (car answers))))
    (when chosen (cons (car chosen) (cddr chosen)))))

(defun align-buffer-xref--pane-location (location)
  "Return a location in a pane standing for LOCATION, or nil."
  (pcase (align-buffer-xref--location-target location)
    (`(,buffer ,line ,column)
     (when-let* ((session (align-buffer-xref--session-for buffer))
                 (found (align-buffer-xref--pane-position
                         session buffer line column)))
       (xref-make-buffer-location
        (align-buffer-buffer session (cdr found)) (car found))))))

(defun align-buffer-xref--remap (items)
  "Return ITEMS, with every answer a pane shows moved into that pane.
Changed in place, so a backend's own item class survives."
  (when align-buffer-xref-remap-results
    (dolist (item items)
      (when (xref-item-p item)
        (when-let ((location (align-buffer-xref--pane-location
                              (xref-item-location item))))
          (setf (xref-item-location item) location)))))
  items)


;;; Enabling

(defun align-buffer-xref--enable (session)
  "Let each of SESSION's panes answer xref through its sources."
  (dolist (side '(left right))
    (with-current-buffer (align-buffer-buffer session side)
      (add-hook 'xref-backend-functions #'align-buffer-xref-backend nil t))))

;; On the build hook, because entering the pane's major mode clears buffer-local
;; hooks and a rebuild enters it again.
(add-hook 'align-buffer-post-build-functions #'align-buffer-xref--enable)

(provide 'align-buffer-xref)

;;; align-buffer-xref.el ends here
