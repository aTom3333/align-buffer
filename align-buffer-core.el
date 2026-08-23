;;; align-buffer-core.el --- Types and maps for align-buffer -*- lexical-binding: t; -*-

;;; Commentary:

;; The alignment plan, the session built from it, and the map between a pane row
;; and the source line it stands for.  Every other file builds on these.
;;
;; A pane holds exactly one real line per row, so a row index is a pane line
;; number minus one.  That is why the map needs no table from positions back to
;; rows.

;;; Code:

(require 'cl-lib)
(require 'seq)

(defgroup align-buffer nil
  "Show two buffers side by side with their lines aligned."
  :group 'convenience
  :prefix "align-buffer-")


;;; Customization

(defcustom align-buffer-show-gutter t
  "When non-nil, show a line-number gutter beside each pane."
  :type 'boolean
  :group 'align-buffer)

(defcustom align-buffer-copied-locals '(tab-width)
  "Buffer-local variables a pane copies from its source buffer."
  :type '(repeat variable)
  :group 'align-buffer)

(defcustom align-buffer-copied-text-properties
  '(face font-lock-face syntax-table)
  "Text properties kept when a source line is copied into a pane.
Everything else is dropped, `category' included: it is an indirection whose
symbol plist can supply further properties."
  :type '(repeat symbol)
  :group 'align-buffer)

(defface align-buffer-gutter '((t :inherit line-number))
  "Face for the line numbers beside a pane."
  :group 'align-buffer)


;;; The plan

(cl-defstruct (align-buffer-cell (:constructor align-buffer-cell-create)
                                 (:copier nil))
  "One side of one row.

KIND is `blank', `line' or `text', and it decides which of the rest apply:

  blank  padding.  The pane gets an empty line and no gutter number.
  line   SOURCE, a buffer, and LINE, its one-based line number.
  text   TEXT, a string with no newline, standing for no line of any buffer.

NUMBER is an integer to show in the gutter, defaulting to LINE.  FACE is a face
or a list of faces, covering the whole row.  REFINEMENT is a list of (BEG . END)
character offsets into the row's text.

The slots a KIND does not use are nil, and no consumer may read them: a `blank'
cell has no SOURCE, and a `text' cell's LINE means nothing.  Emacs Lisp has no
sum type to make that unrepresentable, so it is an invariant to keep rather than
one the compiler keeps for us."
  (kind 'blank)
  source
  line
  text
  number
  face
  refinement)

(cl-defstruct (align-buffer-row (:constructor align-buffer-row-create)
                                (:copier nil))
  "One aligned row: what the two panes show on the same screen line.

LEFT and RIGHT are `align-buffer-cell' structs; a side left unspecified shows
nothing, so a row is well formed with neither.

TAG is any symbol, and is opaque to us.  Non-nil marks the row as interesting,
and a run of adjacent rows whose tags are `eq' becomes one section.  That covers
the common case; a generator wanting sections that overlap or nest supplies them
through the plan instead, since no tagging of single rows can express those."
  (left (align-buffer-cell-create))
  (right (align-buffer-cell-create))
  tag)

(cl-defstruct (align-buffer-plan (:constructor align-buffer-plan-create)
                                 (:copier nil))
  "An ordered set of rows plus what the panes need in order to show them.

ROWS is a vector or a list of `align-buffer-row'.  LEFT-NAME and RIGHT-NAME are
strings.  LEFT-PARAMETERS and RIGHT-PARAMETERS are plists as built by
`align-buffer-parameters-from-buffer'.  SECTIONS is a vector or list of
\(FIRST-ROW . LAST-ROW) pairs, and PROPERTIES a plist.

SECTIONS may overlap and nest: a moved block and the hunks inside it are all
sections.  Supplying it replaces what the rows' tags would have said."
  rows
  left-name
  right-name
  left-parameters
  right-parameters
  sections
  properties)

(cl-defstruct (align-buffer-session (:constructor align-buffer-session-create)
                                    (:copier nil))
  "A built plan: the two panes and the map between rows and source lines.

LEFT-BUFFER and RIGHT-BUFFER are the pane buffers, and they are the session's
identity.  LEFT-POSITIONS and RIGHT-POSITIONS are vectors of buffer positions,
indexed by row.  SOURCE-ROWS is a hash table from a (BUFFER . LINE) cons to the
list of (ROW-INDEX . SIDE) pairs showing that line, which is one-to-many.
SOURCES is a hash table from a source buffer to the list of sides reading it.
SECTIONS is a vector of (FIRST-ROW . LAST-ROW) pairs.  HEIGHTS is empty while
every row is one screen line.  DECORATIONS and PROPERTIES are alists for
extensions."
  plan
  left-buffer
  right-buffer
  left-positions
  right-positions
  source-rows
  sources
  left-gutter-width
  right-gutter-width
  sections
  heights
  decorations
  properties)


;;; Pane-local state

(defvar-local align-buffer--session nil
  "The session this pane belongs to.")

(defvar-local align-buffer--side nil
  "Which side of its session this pane is, `left' or `right'.")


;;; The registry

(defvar align-buffer--sessions nil
  "Every live session.")

(defun align-buffer-sessions ()
  "Return the live sessions, forgetting any that have died."
  (setq align-buffer--sessions
        (seq-filter #'align-buffer-session-live-p align-buffer--sessions)))

(defun align-buffer-session-live-p (session)
  "Return non-nil when both of SESSION's panes are still alive."
  (and (buffer-live-p (align-buffer-session-left-buffer session))
       (buffer-live-p (align-buffer-session-right-buffer session))))

(defun align-buffer-current-session ()
  "Return the session of the current buffer's pane, or nil outside one."
  (and align-buffer--session
       (align-buffer-session-live-p align-buffer--session)
       align-buffer--session))

(defun align-buffer-sessions-for-source (buffer)
  "Return the live sessions showing any line of BUFFER."
  (seq-filter (lambda (session)
                (memq buffer (align-buffer-sources session)))
              (align-buffer-sessions)))


;;; Reading the plan

(defun align-buffer-rows (session)
  "Return SESSION's rows."
  (align-buffer-plan-rows (align-buffer-session-plan session)))

(defun align-buffer-row-count (session)
  "Return how many rows SESSION has."
  (length (align-buffer-rows session)))

(defun align-buffer-row (session row-index)
  "Return SESSION's row at ROW-INDEX, or nil when it is out of range."
  (let ((rows (align-buffer-rows session)))
    (and (>= row-index 0) (< row-index (length rows)) (aref rows row-index))))

(defun align-buffer-row-cell (row side)
  "Return SIDE's cell of ROW, which is a row struct."
  (if (eq side 'left) (align-buffer-row-left row) (align-buffer-row-right row)))

(defun align-buffer-cell (session row-index side)
  "Return SIDE's cell of SESSION's row at ROW-INDEX."
  (let ((row (align-buffer-row session row-index)))
    (and row (align-buffer-row-cell row side))))

(defun align-buffer-other-side (side)
  "Return the side facing SIDE."
  (if (eq side 'left) 'right 'left))

(defun align-buffer-buffer (session side)
  "Return SESSION's pane buffer for SIDE."
  (if (eq side 'left)
      (align-buffer-session-left-buffer session)
    (align-buffer-session-right-buffer session)))

(defun align-buffer-positions (session side)
  "Return SESSION's vector of row positions for SIDE."
  (if (eq side 'left)
      (align-buffer-session-left-positions session)
    (align-buffer-session-right-positions session)))

(defun align-buffer-parameters (plan side)
  "Return PLAN's parameters for SIDE."
  (if (eq side 'left)
      (align-buffer-plan-left-parameters plan)
    (align-buffer-plan-right-parameters plan)))

(defun align-buffer-cell-gutter-number (cell)
  "Return the gutter number CELL should show, or nil for none."
  (or (align-buffer-cell-number cell)
      (and (eq (align-buffer-cell-kind cell) 'line)
           (align-buffer-cell-line cell))))


;;; The map

(defun align-buffer-row-at-point (&optional position)
  "Return the row index shown at POSITION, defaulting to point.
Valid in a pane, for any POSITION inside the row."
  (let ((position (or position (point)))
        (positions (and align-buffer--session
                        align-buffer--side
                        (align-buffer-positions align-buffer--session
                                                align-buffer--side))))
    (if (and positions (> (length positions) 0))
        (align-buffer--row-for-position positions position)
      (1- (line-number-at-pos position)))))

(defun align-buffer--row-for-position (positions position)
  "Return the index of the last of POSITIONS at or before POSITION."
  (let ((low 0)
        (high (1- (length positions))))
    (while (< low high)
      (let ((middle (/ (+ low high 1) 2)))
        (if (<= (aref positions middle) position)
            (setq low middle)
          (setq high (1- middle)))))
    low))

(defun align-buffer-row-beginning-position (session row-index side)
  "Return where SESSION's row ROW-INDEX begins in SIDE's pane.
A buffer position in that pane, of the row's first character, as
`line-beginning-position' would give for the line."
  (let ((positions (align-buffer-positions session side)))
    (and positions (>= row-index 0) (< row-index (length positions))
         (aref positions row-index))))

(defun align-buffer-clamp-row (session row-index)
  "Return ROW-INDEX brought inside SESSION's range."
  (max 0 (min (1- (align-buffer-row-count session)) row-index)))

(defun align-buffer-source-rows (session buffer line)
  "Return the (ROW-INDEX . SIDE) pairs of SESSION showing BUFFER's LINE."
  (gethash (cons buffer line) (align-buffer-session-source-rows session)))

(defun align-buffer-sources (session &optional side)
  "Return the buffers SESSION reads lines from, on SIDE or on both."
  (let ((sources (align-buffer-session-sources session))
        (found nil))
    (when sources
      (maphash (lambda (buffer sides)
                 (when (or (null side) (memq side sides))
                   (push buffer found)))
               sources))
    (nreverse found)))

(defun align-buffer-source-line-position (buffer line &optional offset)
  "Return the position of BUFFER's LINE, OFFSET characters in."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (save-excursion
          (goto-char (point-min))
          (forward-line (1- line))
          (min (+ (point) (or offset 0)) (line-end-position)))))))

(defun align-buffer-source-position (session row-index side &optional offset)
  "Return (BUFFER . POSITION) for SESSION's ROW-INDEX on SIDE, OFFSET in.
Nil when that side of the row shows no source line."
  (let ((cell (align-buffer-cell session row-index side)))
    (when (and cell (eq (align-buffer-cell-kind cell) 'line))
      (let* ((buffer (align-buffer-cell-source cell))
             (position (align-buffer-source-line-position
                        buffer (align-buffer-cell-line cell) offset)))
        (and position (cons buffer position))))))

(defun align-buffer-pane-positions (session buffer line &optional offset)
  "Return where BUFFER's LINE appears in SESSION's panes, OFFSET characters in.
Each element is (POSITION ROW-INDEX . SIDE); empty when no row shows that line."
  (mapcar (lambda (pair)
            (let* ((row-index (car pair))
                   (side (cdr pair))
                   (start (align-buffer-row-beginning-position
                           session row-index side))
                   (limit (align-buffer--row-end session row-index side)))
              (cons (min (+ start (or offset 0)) limit) pair)))
          (align-buffer-source-rows session buffer line)))

(defun align-buffer--row-end (session row-index side)
  "Return where SESSION's row ROW-INDEX ends in SIDE's pane."
  (let ((next (align-buffer-row-beginning-position session (1+ row-index) side)))
    (if next
        (1- next)
      (with-current-buffer (align-buffer-buffer session side)
        (max (point-min) (1- (point-max)))))))


;;; Parameters

(defun align-buffer-parameters-from-buffer (buffer)
  "Return the pane parameters a pane showing BUFFER's lines should use."
  (with-current-buffer buffer
    (list :syntax-table (syntax-table)
          :parse-sexp-lookup-properties parse-sexp-lookup-properties
          :locals (delq nil
                        (mapcar (lambda (variable)
                                  (and (boundp variable)
                                       (cons variable (symbol-value variable))))
                                align-buffer-copied-locals)))))


(defun align-buffer--effective-margin (window)
  "Return the rows `scroll-margin' actually keeps clear in WINDOW."
  (let ((height (window-body-height window)))
    (min (buffer-local-value 'scroll-margin (window-buffer window))
         (max 0 (truncate (* maximum-scroll-margin height))))))


;;; Sections

(defun align-buffer-derive-sections (rows)
  "Return a vector of (FIRST . LAST) runs of adjacent ROWS sharing a tag.
ROWS may be a vector or a list, as a plan's rows may be."
  (let ((rows (if (vectorp rows) rows (vconcat rows)))
        (sections nil)
        (first nil)
        (tag nil)
        (count (length rows)))
    (dotimes (row-index count)
      (let ((current (align-buffer-row-tag (aref rows row-index))))
        (cond
         ((and first (eq current tag)))
         (t
          (when first (push (cons first (1- row-index)) sections))
          (setq first (and current row-index))
          (setq tag current)))))
    (when first (push (cons first (1- count)) sections))
    (vconcat (nreverse sections))))

(provide 'align-buffer-core)

;;; align-buffer-core.el ends here
