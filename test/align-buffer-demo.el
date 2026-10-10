;;; align-buffer-demo.el --- A hand-written correspondence to look at -*- lexical-binding: t; -*-

;;; Commentary:

;; `M-x align-buffer-demo' shows two real buffers related by a correspondence
;; written out by hand, so the sync can be watched without a diff behind it.
;;
;; Both buffers are built from a list of line identities, and a line's text
;; follows from its identity.  Two lines are paired exactly when they are the
;; same identity, so the alignment can be checked by eye: whatever sits level
;; carries the same name.
;;
;; Most identities read the same on both sides, word for word.  The changed ones
;; do not: they keep their name and differ in length, so one side takes three
;; screen rows where the other takes one.  That is the case where a row cannot
;; stand for a line on both sides at once.
;;
;; The two lists between them have every shape worth looking at.  A long
;; stretch in the same order on both sides, a run only the old side has, a much
;; longer run only the new side has, a block whose two halves sit far apart, and
;; a shared stretch at the end the way a real diff usually finishes.  The text
;; of a line says which of those it belongs to.
;;
;; The buffers are wrapped, so a long line takes several screen rows.  A paired
;; line still has one row in each window, but the rows above it no longer match
;; up line for line.
;;
;; A few lines carry a note above them, hung on an overlay the way a checker or
;; a blame header hangs one.  Those rows are display and not text, they are on
;; one side only, and no position in the buffer stands anywhere inside them.

;;; Code:

(require 'cl-lib)
(require 'align-buffer)

(defconst align-buffer-demo--removed-base 1000
  "Identities above this and below the moved ones belong to the old side only.")

(defconst align-buffer-demo--moved-base 2000
  "Identities above this and below the added ones belong to the moved block.")

(defconst align-buffer-demo--added-base 3000
  "Identities above this and below the grown ones belong to the new side only.")

(defconst align-buffer-demo--grown-base 4000
  "Identities above this are short on the old side and long on the new.")

(defconst align-buffer-demo--shrunk-base 5000
  "Identities above this are long on the old side and short on the new.")

(defun align-buffer-demo--long-tail (words)
  "Return a comment long enough to take WORDS words of a screen row."
  (concat " // " (mapconcat #'identity (make-list words "and so on") " ")))

(defun align-buffer-demo--text (id side)
  "Return the text of the line called ID as SIDE shows it.
A line paired across the two sides reads the same on both, save for the changed
ones, which keep their name and differ in length so that one side wraps where
the other does not."
  (cond
   ((> id align-buffer-demo--shrunk-base)
    (let ((number (- id align-buffer-demo--shrunk-base)))
      (format "\tchanged_shrunk(%d);%s" number
              (if (eq side 'left) (align-buffer-demo--long-tail 25) ""))))
   ((> id align-buffer-demo--grown-base)
    (let ((number (- id align-buffer-demo--grown-base)))
      (format "\tchanged_grown(%d);%s" number
              (if (eq side 'right) (align-buffer-demo--long-tail 25) ""))))
   ((> id align-buffer-demo--added-base)
    (format "\tadded_in_new(%d);" (- id align-buffer-demo--added-base)))
   ((> id align-buffer-demo--moved-base)
    (format "\tmoved_block(%d);" (- id align-buffer-demo--moved-base)))
   ((> id align-buffer-demo--removed-base)
    (format "\tremoved_from_old(%d);" (- id align-buffer-demo--removed-base)))
   ((zerop (% id 20))
    (format "\t// shared %d%s" id (align-buffer-demo--long-tail 12)))
   ((zerop (% id 7))
    (format "\tint shared_%d = %d;" id id))
   (t
    (format "\tcall_shared(%d);" id))))

(defun align-buffer-demo--old-ids ()
  "Return the identities of the old side's lines, in order."
  (append (number-sequence 1 80)
          (number-sequence 1001 1040)
          (number-sequence 81 120)
          (number-sequence 4001 4010)
          (number-sequence 5001 5010)
          (number-sequence 121 140)
          (number-sequence 2001 2020)
          (number-sequence 141 170)
          (number-sequence 171 200)))

(defun align-buffer-demo--new-ids ()
  "Return the identities of the new side's lines, in order."
  (append (number-sequence 1 80)
          (number-sequence 81 120)
          (number-sequence 4001 4010)
          (number-sequence 5001 5010)
          (number-sequence 121 140)
          (number-sequence 141 170)
          (number-sequence 3001 3120)
          (number-sequence 2001 2020)
          (number-sequence 171 200)))

(defconst align-buffer-demo--notes
  '((left 40 1) (right 90 2) (left 165 1) (right 135 2) (left 275 3))
  "Where to hang a note, as (SIDE LINE ROWS).
ROWS screen rows of it sit above LINE, on SIDE only, so those rows belong to
one window and have no counterpart in the other.")

(defun align-buffer-demo--annotate (buffer line rows)
  "Hang a note of ROWS screen rows above LINE of BUFFER.
The note is a `before-string', the way a checker or a blame header puts one
there: rows of the display that are no part of the text."
  (with-current-buffer buffer
    (let ((overlay (make-overlay (align-buffer-line-position buffer line)
                                 (align-buffer-line-position buffer line))))
      (overlay-put overlay 'align-buffer-demo-note t)
      (overlay-put
       overlay 'before-string
       (mapconcat (lambda (row)
                    (propertize (format "\t>> a note above line %d, row %d of %d\n"
                                        line (1+ row) rows)
                                'face 'font-lock-warning-face))
                  (number-sequence 0 (1- rows))
                  "")))))

(defun align-buffer-demo--pairs (left-ids right-ids)
  "Return a pair of line numbers for each identity LEFT-IDS and RIGHT-IDS share."
  (let ((places (make-hash-table :test #'eq))
        (line 0)
        (pairs nil))
    (dolist (id left-ids)
      (puthash id (cl-incf line) places))
    (setq line 0)
    (dolist (id right-ids)
      (cl-incf line)
      (when-let ((left (gethash id places)))
        (push (cons left line) pairs)))
    (nreverse pairs)))

(defun align-buffer-demo--buffer (name side ids mode)
  "Return a buffer called NAME holding SIDE's text for the lines IDS, under MODE."
  (let ((buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (remove-overlays)
        (dolist (id ids)
          (insert (align-buffer-demo--text id side) "\n")))
      (delay-mode-hooks (funcall mode))
      (font-lock-mode 1)
      (display-line-numbers-mode 1)
      ;; Wrapped, so every twentieth line takes several screen rows and a row
      ;; stops standing for a line.
      (visual-line-mode 1)
      ;; Five rows kept clear at each edge, which is what redisplay fights the
      ;; sync over when the facing buffer runs out of lines.
      (setq-local scroll-margin 5)
      (set-buffer-modified-p nil)
      (goto-char (point-min)))
    buffer))

;;;###autoload
(defun align-buffer-demo ()
  "Show two buffers related by a hand-written correspondence."
  (interactive)
  (dolist (name '("*align-demo old*" "*align-demo new*"))
    (when-let ((buffer (get-buffer name)))
      (when-let ((session (align-buffer-session-for buffer)))
        (align-buffer-quit session))))
  (let* ((old-ids (align-buffer-demo--old-ids))
         (new-ids (align-buffer-demo--new-ids))
         (old (align-buffer-demo--buffer "*align-demo old*" 'left old-ids
                                         #'prog-mode))
         (new (align-buffer-demo--buffer "*align-demo new*" 'right new-ids
                                         #'prog-mode)))
    (pcase-dolist (`(,side ,line ,rows) align-buffer-demo--notes)
      (align-buffer-demo--annotate (if (eq side 'left) old new) line rows))
    (align-buffer-show old new (align-buffer-demo--pairs old-ids new-ids))
    (message (concat "Paired lines carry the same name, so whatever sits level "
                     "should match.  M-x align-buffer-quit to end."))))

(provide 'align-buffer-demo)

;;; align-buffer-demo.el ends here
