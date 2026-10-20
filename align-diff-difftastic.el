;;; align-diff-difftastic.el --- Read a difference with difftastic -*- lexical-binding: t; -*-

;; Author: Thomas Ferrand
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, vc
;; URL: https://github.com/aTom3333/align-buffer

;;; Commentary:

;; The backend that reads a difference with difftastic
;; (https://difftastic.wilfred.me.uk/), which `align-diff-backend' set to
;; `difftastic' puts in place of the line diff.
;;
;; difftastic parses both sides and compares what they parse to, so what it
;; reports is which syntax changed rather than which lines did: a line
;; rewritten around the same expression still pairs with its own, and the
;; characters it marks are the tokens that differ rather than a stretch of
;; text two line diffs happened to have in common.  A language it has no
;; parser for it compares line by line, which is also what it does with a
;; buffer whose name gives no extension to read a language off.
;;
;; Both sides go to a temp file, difftastic reading files rather than text,
;; and the two files carry one extension - the new side's, that being the name
;; difftastic reads the language off.
;;
;; What it is asked for is its JSON, which gives the whole alignment rather
;; than the hunks a diff gives: an entry for each line of each side, in order,
;; either paired with a line of the other side or facing nothing there.  A
;; section is a stretch of entries the two sides do not agree on, which is
;; what a hunk amounts to once the lines are parsed instead of read.

;;; Code:

(require 'align-diff-core)

(defcustom align-diff-difftastic-command "difft"
  "The difftastic program to run."
  :type 'string
  :group 'align-diff)

(defcustom align-diff-difftastic-switches nil
  "Switches for `align-diff-difftastic-command', on top of the JSON we read.
`--override' is the one that names a language for a file whose extension
difftastic does not know, as in \"--override=*.inc:C\"."
  :type '(repeat string)
  :group 'align-diff)


;;; Running difftastic

(defun align-diff-difftastic--program ()
  "Return the difftastic program to run."
  (or (executable-find align-diff-difftastic-command)
      (error "align-diff: %s is not installed" align-diff-difftastic-command)))

(defun align-diff-difftastic--extension (buffer)
  "Return the extension difftastic should read BUFFER's language off.
`align-diff-language-extension' where the consumer set one, the name of the
file BUFFER visits otherwise, and its own name failing that.  Nil where none
of them gives anything to go by, which leaves difftastic comparing the text
line by line."
  (let ((extension (or (buffer-local-value 'align-diff-language-extension
                                           buffer)
                       (file-name-extension (or (buffer-file-name buffer)
                                                (buffer-name buffer))))))
    (and extension
         (string-match-p "\\`[[:alnum:]_+-]+\\'" extension)
         extension)))

(defun align-diff-difftastic--write (buffer extension)
  "Write BUFFER's text to a temp file named with EXTENSION, and return the file."
  (let ((file (make-temp-file "align-diff-" nil
                              (and extension (concat "." extension))))
        ;; Plain UTF-8, since the offsets difftastic reports count the bytes
        ;; of what it read.
        (coding-system-for-write 'utf-8-unix))
    (with-current-buffer buffer
      (save-restriction
        ;; A reader's narrowing is their view of the buffer, not a statement
        ;; about what to compare.
        (widen)
        (write-region (point-min) (point-max) file nil 'no-message)))
    file))

(defun align-diff-difftastic--report (old-file new-file)
  "Return what difftastic says of OLD-FILE against NEW-FILE, read from its JSON.
Its JSON is behind `DFT_UNSTABLE', which says the shape may change from one
version to the next, so the variable is set for the one call."
  (with-temp-buffer
    (let* ((process-environment (cons "DFT_UNSTABLE=yes" process-environment))
           (status (apply #'call-process (align-diff-difftastic--program)
                          nil t nil
                          (append (list "--display" "json")
                                  align-diff-difftastic-switches
                                  (list old-file new-file)))))
      (unless (eq status 0)
        (error "align-diff: difftastic failed (%S): %s" status
               (string-trim (buffer-string))))
      (goto-char (point-min))
      (json-parse-buffer :object-type 'alist :array-type 'list
                         :null-object nil :false-object nil))))


;;; The lines difftastic changes

(defun align-diff-difftastic--side-key (side)
  "Return what difftastic calls SIDE."
  (if (eq side 'left) 'lhs 'rhs))

(defun align-diff-difftastic--parts (chunks side)
  "Return what CHUNKS say of SIDE, one part for each line they touch of it."
  (let ((key (align-diff-difftastic--side-key side))
        (parts nil))
    (dolist (chunk chunks)
      (dolist (item chunk)
        (when-let ((part (alist-get key item)))
          (push part parts))))
    (nreverse parts)))

(defun align-diff-difftastic--changed (chunks side lines)
  "Return which of SIDE's LINES lines CHUNKS change, one-based and in order."
  (let ((changed nil))
    (dolist (part (align-diff-difftastic--parts chunks side))
      (let ((line (1+ (alist-get 'line_number part))))
        (when (and (alist-get 'changes part) (<= line lines))
          (push line changed))))
    (sort (delete-dups changed) #'<)))

(defun align-diff-difftastic--runs (lines side)
  "Return LINES as the runs of consecutive lines to colour on SIDE."
  (let ((runs nil)
        (first nil)
        (last nil))
    (dolist (line lines)
      (cond ((null first) (setq first line last line))
            ((= line (1+ last)) (setq last line))
            (t (push (list side first (1+ (- last first))) runs)
               (setq first line last line))))
    (when first
      (push (list side first (1+ (- last first))) runs))
    (nreverse runs)))


;;; The characters difftastic marks

(defun align-diff-difftastic--line-text (buffer line)
  "Return LINE of BUFFER, without its newline."
  (let ((position (align-buffer-line-position buffer line)))
    (when position
      (with-current-buffer buffer
        (save-restriction
          (widen)
          (buffer-substring-no-properties
           position (align-diff--line-end buffer line)))))))

(defun align-diff-difftastic--characters (buffer line bytes)
  "Return how many characters into LINE of BUFFER its first BYTES bytes reach.
difftastic counts the bytes of the UTF-8 it was given, where a range of the
core's counts the characters of the line."
  (let* ((text (or (align-diff-difftastic--line-text buffer line) ""))
         (encoded (encode-coding-string text 'utf-8-unix)))
    (length (decode-coding-string
             (substring encoded 0 (min bytes (length encoded)))
             'utf-8-unix))))

(defun align-diff-difftastic--ranges (chunks side buffer lines)
  "Return which characters of SIDE's first LINES lines CHUNKS mark in BUFFER."
  (let ((ranges nil))
    (dolist (part (align-diff-difftastic--parts chunks side))
      (let ((line (1+ (alist-get 'line_number part))))
        (when (<= line lines)
          (dolist (change (alist-get 'changes part))
            (let ((start (alist-get 'start change))
                  (end (alist-get 'end change)))
              (when (> end start)
                (push (list side line
                            (align-diff-difftastic--characters buffer line start)
                            (align-diff-difftastic--characters buffer line end))
                      ranges)))))))
    (nreverse ranges)))


;;; The alignment, and the stretches the two sides disagree over

(defun align-diff-difftastic--entries (aligned old-lines new-lines)
  "Return ALIGNED as the pair of lines each of its entries holds, one-based.
Either of the two is nil where that side has no line facing the other's.
difftastic counts from zero, and counts one line past the end of a file that
ends in a newline, so it can name a line a buffer does not have; an entry
naming none at all is left out."
  (let ((entries nil))
    (dolist (entry aligned)
      (pcase-let* ((`(,left ,right) entry)
                   (left-line (and left (<= (1+ left) old-lines) (1+ left)))
                   (right-line (and right (<= (1+ right) new-lines) (1+ right))))
        (when (or left-line right-line)
          (push (list left-line right-line) entries))))
    (nreverse entries)))

(defun align-diff-difftastic--entry-line (entry side)
  "Return the line SIDE holds in ENTRY, or nil where it holds none."
  (if (eq side 'left) (car entry) (cadr entry)))

(defun align-diff-difftastic--pairs (entries)
  "Return the lines ENTRIES pair up."
  (let ((pairs nil))
    (pcase-dolist (`(,left ,right) entries)
      (when (and left right)
        (push (cons left right) pairs)))
    (nreverse pairs)))

(defun align-diff-difftastic--table (lines)
  "Return LINES as a table to ask membership of."
  (let ((table (make-hash-table :test #'eq)))
    (dolist (line lines)
      (puthash line t table))
    table))

(defun align-diff-difftastic--quiet-p (entry left-changed right-changed)
  "Return non-nil when ENTRY is a line both sides have and neither changed."
  (pcase-let ((`(,left ,right) entry))
    (and left right
         (not (gethash left left-changed))
         (not (gethash right right-changed)))))

(defun align-diff-difftastic--stretches (entries left-changed right-changed)
  "Return the runs of ENTRIES the two sides do not agree on.
Each is (RUN . NEXT), NEXT being the entry that closed the run - a line both
sides have and neither changed - and nil where the run reaches the end."
  (let ((stretches nil)
        (run nil))
    (dolist (entry entries)
      (if (align-diff-difftastic--quiet-p entry left-changed right-changed)
          (when run
            (push (cons (nreverse run) entry) stretches)
            (setq run nil))
        (push entry run)))
    (when run
      (push (cons (nreverse run) nil) stretches))
    (nreverse stretches)))

(defun align-diff-difftastic--span (entries side)
  "Return the lines of SIDE the ENTRIES hold, as (FIRST . COUNT).
Nil where the side holds none of them.  They are consecutive, the alignment
naming a side's lines in order and once each."
  (let ((lines (delq nil (mapcar (lambda (entry)
                                   (align-diff-difftastic--entry-line entry side))
                                 entries))))
    (when lines
      (cons (car lines) (length lines)))))

(defun align-diff-difftastic--sections (stretches old-lines new-lines)
  "Return STRETCHES as the sections of a reading.
A side holding none of a stretch's lines gets the line they belong in front
of, which is the line the entry closing the stretch holds, or one past the
last where the stretch reaches the end."
  (mapcar
   (lambda (stretch)
     (pcase-let* ((`(,entries . ,next) stretch)
                  (left (align-diff-difftastic--span entries 'left))
                  (right (align-diff-difftastic--span entries 'right)))
       (list (if left
                 (car left)
               (or (and next (align-diff-difftastic--entry-line next 'left))
                   (1+ old-lines)))
             (if left (cdr left) 0)
             (if right
                 (car right)
               (or (and next (align-diff-difftastic--entry-line next 'right))
                   (1+ new-lines)))
             (if right (cdr right) 0))))
   stretches))

(defun align-diff-difftastic--stretch-gaps (stretch old-lines new-lines)
  "Return where STRETCH leaves a pane with no lines for the other's.
A gap is one-sided where the pane has none of the stretch's lines at all,
which is the stretch that would otherwise go unmarked: one with lines on both
sides is coloured on both and says so already."
  (let ((gaps nil))
    (dolist (side '(left right))
      (pcase-let* ((`(,entries . ,next) stretch)
                   (other (align-buffer-other-side side))
                   (kind (if (eq side 'left) 'added 'removed))
                   (lines (if (eq side 'left) old-lines new-lines))
                   (one-sided (null (align-diff-difftastic--span entries side)))
                   (facing-first nil)
                   (count 0))
        (dolist (entry (if next (append entries (list next)) entries))
          (let ((own (align-diff-difftastic--entry-line entry side))
                (facing (align-diff-difftastic--entry-line entry other)))
            (if (and (null own) facing)
                (progn
                  (unless facing-first (setq facing-first facing))
                  (setq count (1+ count)))
              ;; The run ends where the pane has a line of its own again.
              (when facing-first
                (push (list side own kind count facing-first one-sided) gaps)
                (setq facing-first nil)
                (setq count 0)))))
        (when facing-first
          (push (list side (1+ lines) kind count facing-first one-sided) gaps))))
    (nreverse gaps)))

(defun align-diff-difftastic--gaps (stretches old-lines new-lines)
  "Return where STRETCHES leave a pane with no lines for the other's."
  (apply #'append
         (mapcar (lambda (stretch)
                   (align-diff-difftastic--stretch-gaps stretch old-lines new-lines))
                 stretches)))


;;; The reading itself

(defun align-diff-difftastic--line-for-line (old-lines new-lines)
  "Return the pairs of two sides difftastic found nothing to say about."
  (let ((pairs nil))
    (dotimes (index (min old-lines new-lines))
      (push (cons (1+ index) (1+ index)) pairs))
    (nreverse pairs)))

(defun align-diff-difftastic--unaligned (status old-lines new-lines)
  "Return the reading a report with no alignment and that STATUS makes.
difftastic aligns nothing where it read one of the two sides as empty, which
it calls created or deleted: the other side's lines then pair nothing, are
coloured whole, and the empty pane is marked in front of its one line.  Any
other status is two sides it found nothing to say about, whose lines pair one
for one."
  (cond
   ((and (equal status "created") (> new-lines 0))
    (list :pairs nil
          :sections (list (list 1 0 1 new-lines))
          :runs (list (list 'right 1 new-lines))
          :gaps (list (list 'left 1 'added new-lines 1 t))))
   ((and (equal status "deleted") (> old-lines 0))
    (list :pairs nil
          :sections (list (list 1 old-lines 1 0))
          :runs (list (list 'left 1 old-lines))
          :gaps (list (list 'right 1 'removed old-lines 1 t))))
   (t
    (list :pairs (align-diff-difftastic--line-for-line old-lines new-lines)))))

(defun align-diff-difftastic--read (report old new)
  "Return the reading REPORT makes of buffers OLD and NEW."
  (let* ((aligned (alist-get 'aligned_lines report))
         (chunks (alist-get 'chunks report))
         (old-lines (align-diff--line-count old))
         (new-lines (align-diff--line-count new)))
    (if (null aligned)
        (align-diff-difftastic--unaligned (alist-get 'status report)
                                          old-lines new-lines)
      (let* ((entries (align-diff-difftastic--entries aligned old-lines new-lines))
             (left-changed (align-diff-difftastic--changed chunks 'left old-lines))
             (right-changed (align-diff-difftastic--changed chunks 'right new-lines))
             (stretches (align-diff-difftastic--stretches
                         entries
                         (align-diff-difftastic--table left-changed)
                         (align-diff-difftastic--table right-changed))))
        (list :pairs (align-diff-difftastic--pairs entries)
              :sections (align-diff-difftastic--sections
                         stretches old-lines new-lines)
              :runs (append (align-diff-difftastic--runs left-changed 'left)
                            (align-diff-difftastic--runs right-changed 'right))
              :gaps (align-diff-difftastic--gaps stretches old-lines new-lines)
              :refine
              (cons 'ranges
                    (append (align-diff-difftastic--ranges
                             chunks 'left old old-lines)
                            (align-diff-difftastic--ranges
                             chunks 'right new new-lines))))))))

(defun align-diff-difftastic--reading (old new)
  "Return difftastic's reading of buffer OLD against buffer NEW."
  (let* ((extension (or (align-diff-difftastic--extension new)
                        (align-diff-difftastic--extension old)))
         (old-file (align-diff-difftastic--write old extension))
         (new-file (align-diff-difftastic--write new extension)))
    (unwind-protect
        (align-diff-difftastic--read
         (align-diff-difftastic--report old-file new-file) old new)
      (delete-file old-file)
      (delete-file new-file))))

(align-diff-register-backend 'difftastic #'align-diff-difftastic--reading)

(provide 'align-diff-difftastic)

;;; align-diff-difftastic.el ends here
