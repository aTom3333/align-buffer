;;; align-diff.el --- Align two buffers by their differences -*- lexical-binding: t; -*-

;; Author: Thomas Ferrand
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, vc
;; URL: https://github.com/aTom3333/align-buffer

;;; Commentary:

;; A plan generator for align-buffer.  `align-diff-buffers' diffs two buffers
;; and shows them side by side with their lines aligned: one row per displayed
;; line, padding on the side that has nothing, and the diff colours on the rows
;; that differ.  A run of differing rows is one section, so align-buffer's
;; section navigation moves hunk to hunk.
;;
;; The two buffers are given rather than made.  They already carry the major mode
;; and the fontification the panes copy, and reading a git revision or a file
;; into a buffer is the business of an entry point built on top of this one.

;;; Code:

(require 'cl-lib)
(require 'align-buffer)
(require 'diff)
(require 'smerge-mode)

(defgroup align-diff nil
  "Two buffers side by side, aligned by their differences."
  :group 'align-buffer
  :prefix "align-diff-")

(defcustom align-diff-switches '("--strip-trailing-cr")
  "Switches for `diff-command', on top of the unified format align-diff needs."
  :type '(repeat string)
  :group 'align-diff)

(defcustom align-diff-refine-hunks t
  "When non-nil, mark which characters differ inside a hunk."
  :type 'boolean
  :group 'align-diff)

(defface align-diff-removed '((t :inherit diff-removed :extend t))
  "A line only the old side has, or the old side of a changed line.")

(defface align-diff-added '((t :inherit diff-added :extend t))
  "A line only the new side has, or the new side of a changed line.")

(defface align-diff-padding-old '((t :inherit diff-removed :extend t))
  "Padding in the old pane, facing a line only the new side has.")

(defface align-diff-padding-new '((t :inherit diff-added :extend t))
  "Padding in the new pane, facing a line only the old side has.")

(defface align-diff-refine-removed '((t :inherit diff-refine-removed))
  "The characters a changed line lost.")

(defface align-diff-refine-added '((t :inherit diff-refine-added))
  "The characters a changed line gained.")


;;; Running diff

(defun align-diff--line-count (buffer)
  "Return how many lines BUFFER holds, narrowing or not."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (count-lines (point-min) (point-max)))))

(defun align-diff--unified (old new)
  "Return the unified diff of buffer OLD against buffer NEW, as a string."
  (let ((output (generate-new-buffer " *align-diff output*"))
        ;; Context wide enough to swallow the whole file, so the diff arrives as
        ;; one hunk and every line of both buffers is accounted for.
        (context (max (align-diff--line-count old) (align-diff--line-count new) 1))
        ;; A reader's narrowing is their view of the buffer, not a statement
        ;; about what to compare.
        (diff-entire-buffers t)
        ;; Both buffers written the same way, and with the line endings Emacs
        ;; shows rather than the platform's: on Windows the default write adds a
        ;; carriage return to every line of both temp files, and
        ;; `--strip-trailing-cr' cannot then rescue a side that holds carriage
        ;; returns of its own, since it strips exactly one.
        (coding-system-for-write 'utf-8-emacs-unix))
    (unwind-protect
        (progn
          (diff-no-select old new
                          (append (list "-u" (format "-U%d" context))
                                  align-diff-switches)
                          t output)
          (with-current-buffer output
            (buffer-substring-no-properties (point-min) (point-max))))
      (kill-buffer output))))


;;; Reading the diff

(defconst align-diff--hunk-header
  "^@@ -\\([0-9]+\\)\\(?:,[0-9]+\\)? \\+\\([0-9]+\\)\\(?:,[0-9]+\\)? @@"
  "Regexp matching a unified hunk header, capturing both starting lines.")

(defun align-diff--operations (diff)
  "Return DIFF as a list of (same OLD NEW), (removed OLD) or (added NEW).
OLD and NEW are one-based line numbers in the buffers that were compared."
  (let ((old-line 0)
        (new-line 0)
        (in-hunk nil)
        (operations nil))
    (dolist (line (split-string diff "\n"))
      (cond
       ((string-match align-diff--hunk-header line)
        (setq in-hunk t
              old-line (1- (string-to-number (match-string 1 line)))
              new-line (1- (string-to-number (match-string 2 line)))))
       ((not in-hunk))
       ;; An empty line ends the body: a line of a hunk always carries its
       ;; prefix character, and an empty context line reads as a single space.
       ;; What follows is the trailer `diff-no-select' appends.
       ((string-empty-p line) (setq in-hunk nil))
       ((string-prefix-p "\\" line))     ; "\ No newline at end of file"
       ((string-prefix-p " " line)
        (push (list 'same (cl-incf old-line) (cl-incf new-line)) operations))
       ((string-prefix-p "-" line)
        (push (list 'removed (cl-incf old-line)) operations))
       ((string-prefix-p "+" line)
        (push (list 'added (cl-incf new-line)) operations))))
    (nreverse operations)))


;;; Building the plan

(defun align-diff--line-cell (buffer line face)
  "Return a cell showing LINE of BUFFER, faced with FACE."
  (align-buffer-cell-create :kind 'line :source buffer :line line :face face))

(defun align-diff--rows (operations old new)
  "Return the rows aligning buffers OLD and NEW according to OPERATIONS."
  (let ((rows nil))
    (while operations
      (pcase (caar operations)
        ('same
         (pcase-let ((`(same ,old-line ,new-line) (pop operations)))
           (push (align-buffer-row-create
                  :left (align-diff--line-cell old old-line nil)
                  :right (align-diff--line-cell new new-line nil))
                 rows)))
        (_
         ;; A run of removals and additions is one edit, however the two are
         ;; interleaved, so they pair up by position: as many changed rows as
         ;; the shorter side has, and what is left over stands alone.
         (let ((removed nil)
               (added nil))
           (while (memq (caar operations) '(removed added))
             (pcase-let ((`(,kind ,line) (pop operations)))
               (if (eq kind 'removed)
                   (push line removed)
                 (push line added))))
           (setq removed (nreverse removed)
                 added (nreverse added))
           (let ((paired (min (length removed) (length added))))
             (dotimes (index paired)
               (push (align-buffer-row-create
                      :left (align-diff--line-cell old (nth index removed)
                                                    'align-diff-removed)
                      :right (align-diff--line-cell new (nth index added)
                                                    'align-diff-added)
                      :tag 'change)
                     rows))
             (dolist (line (nthcdr paired removed))
               (push (align-buffer-row-create
                      :left (align-diff--line-cell old line 'align-diff-removed)
                      :right (align-buffer-cell-create
                              :kind 'blank :face 'align-diff-padding-new)
                      :tag 'change)
                     rows))
             (dolist (line (nthcdr paired added))
               (push (align-buffer-row-create
                      :left (align-buffer-cell-create
                             :kind 'blank :face 'align-diff-padding-old)
                      :right (align-diff--line-cell new line 'align-diff-added)
                      :tag 'change)
                     rows)))))))
    (nreverse rows)))

(defun align-diff-plan (old new &optional properties)
  "Return an align-buffer plan aligning buffer OLD against buffer NEW.
PROPERTIES are carried on the plan.
Signals a `user-error' when the two hold the same text."
  (dolist (buffer (list old new))
    (unless (buffer-live-p buffer)
      (error "Not a live buffer: %S" buffer)))
  (let ((operations (align-diff--operations (align-diff--unified old new))))
    (when (null operations)
      (user-error "No differences between %s and %s"
                  (buffer-name old) (buffer-name new)))
    (align-buffer-plan-create
     :rows (align-diff--rows operations old new)
     :left-name (format "*align-diff old: %s*" (buffer-name old))
     :right-name (format "*align-diff new: %s*" (buffer-name new))
     :left-parameters (align-buffer-parameters-from-buffer old)
     :right-parameters (align-buffer-parameters-from-buffer new)
     :properties (append (list :align-diff t) properties))))

;;; Refining the hunks

(defun align-diff--side-reads-a-line-p (session first last side)
  "Return non-nil when a row between FIRST and LAST reads a line on SIDE."
  (cl-loop for row-index from first to last
           thereis (eq (align-buffer-cell-kind
                        (align-buffer-cell session row-index side))
                       'line)))

(defun align-diff--region (session first last side)
  "Return (BEGINNING . END) for rows FIRST to LAST of SESSION's SIDE pane.
BEGINNING is a marker: a bare position becomes one in the current buffer."
  (with-current-buffer (align-buffer-buffer session side)
    (cons (copy-marker (align-buffer-row-beginning-position session first side))
          (save-excursion
            (goto-char (align-buffer-row-beginning-position session last side))
            (line-end-position)))))

(defun align-diff--refine-hunk (session section)
  "Mark how the two sides of SECTION differ, character by character.
Across the whole hunk, so text moved to another line still corresponds."
  (let ((first (car section))
        (last (cdr section)))
    ;; A hunk with lines on one side only has nothing to correspond with, and
    ;; its rows already carry the colour that says so.
    (when (and (align-diff--side-reads-a-line-p session first last 'left)
               (align-diff--side-reads-a-line-p session first last 'right))
      (let ((left (align-diff--region session first last 'left))
            (right (align-diff--region session first last 'right)))
        ;; Demoted, because refining is a nicety: `smerge-refine-regions' runs
        ;; `diff-command' and reads its output back, and neither is ours.
        (with-demoted-errors "align-diff: refining a hunk failed: %S"
          (smerge-refine-regions
           (car left) (cdr left) (car right) (cdr right)
           nil nil
           '((face . align-diff-refine-removed) (align-diff-refinement . t))
           '((face . align-diff-refine-added) (align-diff-refinement . t))))))))

(defun align-diff--refine (session)
  "Refine every hunk of SESSION."
  (dolist (section (append (align-buffer-session-sections session) nil))
    (align-diff--refine-hunk session section)))

(defun align-diff--post-build (session)
  "Refine SESSION when align-diff made its plan and refining is on."
  (when (and align-diff-refine-hunks
             (plist-get (align-buffer-plan-properties
                         (align-buffer-session-plan session))
                        :align-diff))
    (align-diff--refine session)))

;; On the build hook rather than in the entry point, so a rebuild refines the
;; panes again: rendering erases them, and the overlays go with the text.
(add-hook 'align-buffer-post-build-functions #'align-diff--post-build)


;;; Showing them

;;;###autoload
(defun align-diff-buffers (old new &optional properties)
  "Show buffers OLD and NEW side by side, aligned by their differences.
PROPERTIES are carried on the plan.  Returns the align-buffer session."
  (interactive
   (let ((new (read-buffer "Diff new buffer: " (current-buffer) t)))
     (list (read-buffer "Diff old buffer: "
                        (other-buffer (get-buffer new) t) t)
           new)))
  (align-buffer-show
   (align-diff-plan (get-buffer old) (get-buffer new) properties)))

;;;###autoload
(defun align-diff-files (old new)
  "Show files OLD and NEW side by side, aligned by their differences.
A file already in a buffer is read from there.  One opened for this is killed
with the session."
  (interactive "fOld file: \nfNew file: ")
  (let (opened)
    (cl-flet ((buffer-for (file)
                (or (find-buffer-visiting file)
                    (let ((buffer (find-file-noselect file)))
                      (with-current-buffer buffer (font-lock-mode 1))
                      (push buffer opened)
                      buffer))))
      (let ((old-buffer (buffer-for old))
            (new-buffer (buffer-for new)))
        (align-diff-buffers old-buffer new-buffer
                            (list :owned-buffers opened))))))

(provide 'align-diff)

;;; align-diff.el ends here
