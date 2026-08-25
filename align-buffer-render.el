;;; align-buffer-render.el --- Build align-buffer's panes -*- lexical-binding: t; -*-

;;; Commentary:

;; Turn a plan into two synthetic read-only buffers, one real line per row, with
;; the text and the faces copied out of each row's source buffer.
;;
;; We copy faces rather than fontifying the pane, because the padding lines we
;; insert change the parse for any language where a blank line is significant.
;; Font-lock stays off; the copied `face' property is what redisplay reads.
;;
;; Two mechanisms, and they do not compete.  The source's own highlighting
;; arrives as TEXT PROPERTIES, copied along with the string.  OVERLAYS are only
;; ever the plan's or ours: one per row the plan gives a face to, and one per row
;; for the gutter.  So a row's colour sits underneath the source's highlighting
;; rather than fighting it for the same characters.
;;
;; No key bindings here, or anywhere in the package.  The commands are the
;; interface, and binding them belongs to whoever builds something on top or to
;; the reader's own configuration.  That is also what keeps this file from
;; needing anything out of the lifecycle or the navigation.

;;; Code:

(require 'cl-lib)
(require 'align-buffer-core)
(require 'align-buffer-sync)

(defun align-buffer--pane-display-settings ()
  "Assert the display settings a pane cannot work without."
  (setq-local truncate-lines t)
  (setq-local display-line-numbers nil))


(define-derived-mode align-buffer-pane-mode fundamental-mode "AlignBuffer"
  "Major mode of an align-buffer pane.
The pane runs no language mode: it takes its syntax table and its locals from
the plan's parameters, and its faces come copied with the text."
  :after-hook (align-buffer--pane-display-settings)
  (setq-local buffer-undo-list t)
  (setq-local buffer-read-only t)
  (setq-local char-property-alias-alist '((face font-lock-face)))
  (setq-local fringes-outside-margins t))


;;; Copying a source line

(defun align-buffer--line-starts (buffer)
  "Return a vector of BUFFER's line-beginning positions.
Element zero holds where line one begins, element one where line two begins, and
so on: a zero-based index over one-based line numbers."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (save-match-data
        (save-excursion
          (goto-char (point-min))
          (let ((starts (list (point-min))))
            (while (search-forward "\n" nil t)
              (push (point) starts))
            (vconcat (nreverse starts))))))))

(defun align-buffer--source-starts (rows)
  "Return a hash of source buffer to line starts, for every source ROWS read."
  (let ((starts (make-hash-table :test #'eq)))
    (dotimes (row-index (length rows))
      (dolist (side '(left right))
        (let ((cell (align-buffer-row-cell (aref rows row-index) side)))
          (when (eq (align-buffer-cell-kind cell) 'line)
            (let ((source (align-buffer-cell-source cell)))
              (when (and (buffer-live-p source) (not (gethash source starts)))
                (with-current-buffer source
                  (save-restriction (widen) (font-lock-ensure)))
                (puthash source (align-buffer--line-starts source) starts)))))))
    starts))

(defun align-buffer--kept-properties (properties)
  "Return PROPERTIES restricted to `align-buffer-copied-text-properties'."
  (let ((kept nil))
    (while properties
      (when (memq (car properties) align-buffer-copied-text-properties)
        (setq kept (cons (car properties) (cons (cadr properties) kept))))
      (setq properties (cddr properties)))
    kept))

(defun align-buffer--filter-properties (text)
  "Drop every text property of TEXT we do not copy, and return TEXT."
  (let ((position 0)
        (end (length text)))
    (while (< position end)
      (let ((next (or (next-property-change position text) end)))
        (set-text-properties position next
                             (align-buffer--kept-properties
                              (text-properties-at position text))
                             text)
        (setq position next))))
  text)

(defun align-buffer--line-text (buffer starts line)
  "Return LINE of BUFFER with only the properties we copy.
STARTS is that buffer's line-start vector."
  (let ((index (1- line)))
    (if (or (< index 0) (>= index (length starts)))
        ""
      (let ((beginning (aref starts index))
             (end (if (< (1+ index) (length starts))
                      (1- (aref starts (1+ index)))
                    (with-current-buffer buffer
                      (save-restriction (widen) (point-max))))))
        (align-buffer--filter-properties
         (with-current-buffer buffer
           (save-restriction
             (widen)
             (buffer-substring beginning (max beginning end)))))))))

(defun align-buffer--cell-text (cell starts)
  "Return the text CELL puts on its row.  STARTS is the source-starts hash."
  (pcase (align-buffer-cell-kind cell)
    ('line (let ((source (align-buffer-cell-source cell)))
             (if (buffer-live-p source)
                 (align-buffer--line-text
                  source (gethash source starts) (align-buffer-cell-line cell))
               "")))
    ('text (or (align-buffer-cell-text cell) ""))
    (_ "")))


;;; Parameters and the gutter

(defun align-buffer--apply-parameters (parameters)
  "Apply PARAMETERS to the current pane."
  (when-let ((table (plist-get parameters :syntax-table)))
    (set-syntax-table table))
  (setq-local parse-sexp-lookup-properties
              (plist-get parameters :parse-sexp-lookup-properties))
  (pcase-dolist (`(,variable . ,value) (plist-get parameters :locals))
    (set (make-local-variable variable) value)))

(defun align-buffer--gutter-width (rows side)
  "Return the gutter width SIDE needs for ROWS, or 0 when it needs none."
  (let ((widest 0))
    (dotimes (row-index (length rows))
      (let ((number (align-buffer-cell-gutter-number
                     (align-buffer-row-cell (aref rows row-index) side))))
        (when number (setq widest (max widest number)))))
    (if (zerop widest) 0 (+ 2 (length (number-to-string widest))))))

(defun align-buffer--gutter-string (number width)
  "Return the margin string for NUMBER in WIDTH columns.
A nil NUMBER gives a faced strip with no digits."
  (let* ((digits (if number (number-to-string number) ""))
         (padding (max 0 (- width 1 (length digits)))))
    (propertize
     " " 'display
     `((margin left-margin)
       ,(propertize (concat (make-string padding ?\s) digits " ")
                    'face 'align-buffer-gutter)))))

(defun align-buffer--put-gutter (position number width)
  "Put a gutter overlay at POSITION, showing NUMBER when there is one."
  (let ((overlay (make-overlay position position)))
    (overlay-put overlay 'align-buffer-gutter t)
    (overlay-put overlay 'before-string (align-buffer--gutter-string number width))))

(defun align-buffer--set-gutter-width (width)
  "Give this pane a left margin of WIDTH columns."
  (setq-local left-margin-width (and (> width 0) width))
  (dolist (window (get-buffer-window-list nil nil t))
    (set-window-margins window left-margin-width nil)))


;;; Row faces

(defun align-buffer--put-row-face (face beginning end)
  "Cover BEGINNING to END with FACE."
  (let ((overlay (make-overlay beginning end)))
    (overlay-put overlay 'align-buffer-row t)
    (overlay-put overlay 'face face)))

(defun align-buffer--put-refinement (cell beginning)
  "Cover CELL's refinement ranges, counted from BEGINNING."
  (let ((limit (save-excursion (goto-char beginning) (line-end-position))))
    (pcase-dolist (`(,start ,end ,face) (align-buffer-cell-refinement cell))
      (let ((overlay (make-overlay (min (+ beginning start) limit)
                                   (min (+ beginning end) limit))))
        (overlay-put overlay 'align-buffer-refinement t)
        (overlay-put overlay 'face face)
        ;; Above the row's own face, which covers the same characters.
        (overlay-put overlay 'priority 1)))))


;;; Building a pane

(defun align-buffer--render-pane (session side starts)
  "Build SESSION's pane for SIDE.  STARTS is the source-starts hash.
Return (POSITIONS . GUTTER-WIDTH), the row positions and the gutter's width."
  (let* ((plan (align-buffer-session-plan session))
         (rows (align-buffer-plan-rows plan))
         (count (length rows))
         (positions (make-vector count 0))
         (width (if align-buffer-show-gutter
                    (align-buffer--gutter-width rows side)
                  0))
         (buffer (align-buffer-buffer session side)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (align-buffer-pane-mode)
        (save-restriction
          (widen)
          (dolist (tag '(align-buffer-gutter align-buffer-row
                                            align-buffer-refinement))
            (remove-overlays (point-min) (point-max) tag t)))
        (erase-buffer)
        (align-buffer--apply-parameters (align-buffer-parameters plan side))
        (dotimes (row-index count)
          (aset positions row-index (point))
          (insert (align-buffer--cell-text
                   (align-buffer-row-cell (aref rows row-index) side) starts))
          (insert "\n"))
        (dotimes (row-index count)
          (let* ((cell (align-buffer-row-cell (aref rows row-index) side))
                 (beginning (aref positions row-index))
                 (end (if (< (1+ row-index) count) (aref positions (1+ row-index)) (point-max)))
                 (number (align-buffer-cell-gutter-number cell)))
            (when-let ((face (align-buffer-cell-face cell)))
              (align-buffer--put-row-face face beginning end))
            (when (align-buffer-cell-refinement cell)
              (align-buffer--put-refinement cell beginning))
            (when (> width 0)
              (align-buffer--put-gutter beginning number width))))
        (set-buffer-modified-p nil)
        (goto-char (point-min)))
      (setq align-buffer--session session)
      (setq align-buffer--side side)
      (align-buffer--set-gutter-width width))
    (cons positions width)))

(defun align-buffer--refresh-row-faces (session row-index side starts)
  "Copy the faces of SESSION's row ROW-INDEX in SIDE again, and say whether.
Nothing is copied when the source line no longer reads as the row does.
STARTS is the source's line-start vector."
  (let* ((cell (align-buffer-cell session row-index side))
         (fresh (align-buffer--line-text (align-buffer-cell-source cell)
                                         starts
                                         (align-buffer-cell-line cell)))
         (beginning (align-buffer-row-beginning-position session row-index side)))
    (with-current-buffer (align-buffer-buffer session side)
      (let ((end (save-excursion (goto-char beginning) (line-end-position))))
        ;; Properties only, never the text: a row's overlays - the gutter, the
        ;; row face, a decoration's - hang off these positions.  Which is also
        ;; why the two texts have to be the same one: a source edited since the
        ;; plan was made would have its faces painted over text that no longer
        ;; matches, so a word coloured as a type would sit on whatever the pane
        ;; still shows there.  `string=' compares characters and ignores the
        ;; properties, which is the comparison wanted here.
        (when (string= fresh (buffer-substring-no-properties beginning end))
          (with-silent-modifications
            (set-text-properties beginning end nil)
            (dotimes (offset (length fresh))
              (let ((properties (text-properties-at offset fresh)))
                (when properties
                  (add-text-properties (+ beginning offset)
                                       (+ beginning offset 1)
                                       properties)))))
          t)))))

(defun align-buffer-refresh-faces (session buffer &optional first-line last-line)
  "Copy BUFFER's faces into SESSION's panes again, returning how many rows moved.
Only rows reading BUFFER, and of those only the ones between FIRST-LINE and
LAST-LINE when either is given.  For a fontifier that answers after the panes
were built, a language server's semantic tokens among them."
  (let ((starts (align-buffer--line-starts buffer))
         (rows (align-buffer-rows session))
         (refreshed 0))
    (dolist (side '(left right))
      (dotimes (row-index (length rows))
        (let ((cell (align-buffer-cell session row-index side)))
          (when (and (eq (align-buffer-cell-kind cell) 'line)
                     (eq (align-buffer-cell-source cell) buffer)
                     (or (null first-line)
                         (>= (align-buffer-cell-line cell) first-line))
                     (or (null last-line)
                         (<= (align-buffer-cell-line cell) last-line))
                     (align-buffer--refresh-row-faces session row-index side
                                                      starts))
            (setq refreshed (1+ refreshed))))))
    refreshed))

(defun align-buffer-render (session)
  "Build both of SESSION's panes and fill in its maps."
  (let* ((plan (align-buffer-session-plan session))
         (rows (align-buffer-plan-rows plan))
         (starts (align-buffer--source-starts rows))
         (left (align-buffer--render-pane session 'left starts))
         (right (align-buffer--render-pane session 'right starts)))
    (setf (align-buffer-session-left-positions session) (car left))
    (setf (align-buffer-session-right-positions session) (car right))
    (setf (align-buffer-session-left-gutter-width session) (cdr left))
    (setf (align-buffer-session-right-gutter-width session) (cdr right))
    (setf (align-buffer-session-source-rows session)
          (align-buffer--source-row-table rows))
    (setf (align-buffer-session-sources session)
          (align-buffer--source-table rows))
    (setf (align-buffer-session-sections session)
          (let ((sections (or (align-buffer-plan-sections plan)
                              (align-buffer-derive-sections rows))))
            (if (vectorp sections) sections (vconcat sections))))
    (dolist (side '(left right))
      (with-current-buffer (align-buffer-buffer session side)
        (align-buffer-sync-enable)))
    session))

(defun align-buffer--source-row-table (rows)
  "Return a hash from (BUFFER . LINE) to the (ROW . SIDE) pairs showing it."
  (let ((table (make-hash-table :test #'equal)))
    (dotimes (row-index (length rows))
      (dolist (side '(left right))
        (let ((cell (align-buffer-row-cell (aref rows row-index) side)))
          (when (eq (align-buffer-cell-kind cell) 'line)
            (let ((key (cons (align-buffer-cell-source cell)
                             (align-buffer-cell-line cell))))
              (puthash key (cons (cons row-index side) (gethash key table)) table))))))
    (maphash (lambda (key pairs) (puthash key (nreverse pairs) table)) table)
    table))

(defun align-buffer--source-table (rows)
  "Return a hash from each source buffer to the sides reading it."
  (let ((table (make-hash-table :test #'eq)))
    (dotimes (row-index (length rows))
      (dolist (side '(left right))
        (let ((cell (align-buffer-row-cell (aref rows row-index) side)))
          (when (eq (align-buffer-cell-kind cell) 'line)
            (let* ((source (align-buffer-cell-source cell))
                   (sides (gethash source table)))
              (unless (memq side sides)
                (puthash source (cons side sides) table)))))))
    table))

(provide 'align-buffer-render)

;;; align-buffer-render.el ends here
