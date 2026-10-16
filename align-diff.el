;;; align-diff.el --- Align two buffers by the diff of their lines -*- lexical-binding: t; -*-

;; Author: Thomas Ferrand
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, vc
;; URL: https://github.com/aTom3333/align-buffer

;;; Commentary:

;; The backend that reads a difference by running `diff-command' over the two
;; buffers, which is what `align-diff-buffers' and `align-diff-files' use
;; unless `align-diff-backend' says otherwise.  Everything else align-diff
;; does with a reading is in align-diff-core.
;;
;; A unified diff with no context gives hunks, and the lines pair up from
;; them:
;;
;;   unchanged     every line pairs with the line facing it.
;;   changed       the two sides pair by position, as far as the shorter side
;;                 goes; the lines past that have no counterpart.
;;   one sided     a hunk that is all insertion or all deletion pairs nothing.
;;
;; Each hunk is one of the session's sections, so `align-buffer-next-section'
;; and `align-buffer-previous-section' take the reader hunk to hunk, and a
;; consumer handles running out of them through
;; `align-buffer-section-boundary-functions'.
;;
;; Which characters differ inside a changed hunk is left to
;; `smerge-refine-regions', the hunk being handed over whole so that a word
;; moved to another line of it still corresponds with where it came from.

;;; Code:

(require 'align-diff-core)
(require 'diff)

(defcustom align-diff-switches '("--strip-trailing-cr")
  "Switches for `diff-command', on top of the unified format align-diff needs."
  :type '(repeat string)
  :group 'align-diff)


;;; Running the diff

(defun align-diff--unified (old new)
  "Return the diff of buffer OLD against buffer NEW, as a string.
Without context, so that a header gives the changed lines and nothing else:
what lies between two hunks is the same on both sides and pairs up by counting
rather than by reading."
  (let ((output (generate-new-buffer " *align-diff output*"))
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
          (diff-no-select old new (append (list "-U0") align-diff-switches)
                          t output)
          (with-current-buffer output
            (buffer-substring-no-properties (point-min) (point-max))))
      (kill-buffer output))))

(defconst align-diff--hunk-header
  "^@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@"
  "Regexp matching a unified hunk header, capturing both ranges.")

(defun align-diff--hunks-in (diff)
  "Return the hunks DIFF reports, as (OLD-LINE OLD-COUNT NEW-LINE NEW-COUNT).
Each line is where the hunk begins on that side, counting from one, and each
count is how many lines of it that side has.

A count the header leaves out is one.  A count of zero comes with the line
before the gap, which is where diff says an insertion goes, so the line is
moved on to where the lines would start had the side any."
  (let ((hunks nil))
    (dolist (line (split-string diff "\n"))
      (when (string-match align-diff--hunk-header line)
        (let ((old-line (string-to-number (match-string 1 line)))
              (old-count (if (match-string 2 line)
                             (string-to-number (match-string 2 line))
                           1))
              (new-line (string-to-number (match-string 3 line)))
              (new-count (if (match-string 4 line)
                             (string-to-number (match-string 4 line))
                           1)))
          (push (list (if (zerop old-count) (1+ old-line) old-line) old-count
                      (if (zerop new-count) (1+ new-line) new-line) new-count)
                hunks))))
    (nreverse hunks)))

(defun align-diff--hunks (old new)
  "Return the hunks between buffer OLD and buffer NEW."
  (align-diff--hunks-in (align-diff--unified old new)))


;;; Reading the hunks

(defun align-diff--pairs (hunks old new)
  "Return the pairs aligning buffer OLD against buffer NEW, given HUNKS.
A line outside every hunk pairs with the line facing it.  Inside one the two
sides pair by position as far as the shorter side goes, and the lines past that
have no counterpart, which leaves a hunk with lines on one side only pairing
nothing."
  (let ((old-line 1)
        (new-line 1)
        (pairs nil))
    (dolist (hunk hunks)
      (pcase-let ((`(,old-first ,old-count ,new-first ,new-count) hunk))
        ;; The run up to the hunk, which both sides have line for line.
        (while (< old-line old-first)
          (push (cons old-line new-line) pairs)
          (setq old-line (1+ old-line))
          (setq new-line (1+ new-line)))
        (dotimes (index (min old-count new-count))
          (push (cons (+ old-first index) (+ new-first index)) pairs))
        (setq old-line (+ old-first old-count))
        (setq new-line (+ new-first new-count))))
    ;; And the run after the last hunk, which runs out with the shorter side:
    ;; a buffer whose last line has no newline where the other side's has one
    ;; is a line longer by diff's reckoning than by `count-lines'.
    (let ((old-lines (align-diff--line-count old))
          (new-lines (align-diff--line-count new)))
      (while (and (<= old-line old-lines) (<= new-line new-lines))
        (push (cons old-line new-line) pairs)
        (setq old-line (1+ old-line))
        (setq new-line (1+ new-line))))
    (nreverse pairs)))

(defun align-diff--runs (hunks)
  "Return the lines HUNKS cover, as runs to colour."
  (let ((runs nil))
    (dolist (hunk hunks)
      (pcase-let ((`(,old-first ,old-count ,new-first ,new-count) hunk))
        (when (> old-count 0)
          (push (list 'left old-first old-count) runs))
        (when (> new-count 0)
          (push (list 'right new-first new-count) runs))))
    (nreverse runs)))

(defun align-diff--gaps (hunks)
  "Return where HUNKS leave a pane with no lines for the other's.
A gap is named by what its pane lacks rather than by which pane it is: the old
pane's gap stands for lines the new side gained, and the new pane's for lines
it no longer has.  It belongs below the lines of the hunk that did pair up,
which is where the lines with no counterpart begin."
  (let ((gaps nil))
    (dolist (hunk hunks)
      (pcase-let* ((`(,old-first ,old-count ,new-first ,new-count) hunk)
                   (gap (- old-count new-count))
                   (one-sided (or (zerop old-count) (zerop new-count))))
        (cond
         ((> gap 0)
          (push (list 'right (+ new-first new-count) 'removed gap
                      (+ old-first new-count) one-sided)
                gaps))
         ((< gap 0)
          (push (list 'left (+ old-first old-count) 'added (- gap)
                      (+ new-first old-count) one-sided)
                gaps)))))
    (nreverse gaps)))

(defun align-diff--diff-reading (old new)
  "Return a reading of buffer OLD against buffer NEW from `diff-command'.
The hunks are the sections, and the refinement is left to
`smerge-refine-regions'."
  (let ((hunks (align-diff--hunks old new)))
    (list :pairs (align-diff--pairs hunks old new)
          :sections hunks
          :runs (align-diff--runs hunks)
          :gaps (align-diff--gaps hunks)
          :refine (cons 'regions hunks))))

(align-diff-register-backend 'diff #'align-diff--diff-reading)

(provide 'align-diff)

;;; align-diff.el ends here
