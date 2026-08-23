;;; align-buffer-navigation.el --- Move around align-buffer's panes -*- lexical-binding: t; -*-

;;; Commentary:

;; Section to section, and out to the real position a row stands for.
;;
;; A section is a range of rows the plan calls interesting, either supplied
;; outright or derived from the rows' tags, so what counts as interesting is the
;; generator's business rather than ours.
;;
;; Sections may OVERLAP and NEST, since a generator is free to describe the same
;; rows at more than one size.  So navigation works by section STARTS rather than
;; by stepping through an index: the next section is the one with the earliest
;; start after the current row.  That always moves, whatever the sections
;; overlap.

;;; Code:

(require 'cl-lib)
(require 'align-buffer-core)


;;; Finding the next section

(defun align-buffer--section-after (row-index sections)
  "Return the index of the section to move to from ROW-INDEX, going forwards.
The earliest start greater than ROW-INDEX, and the first in the vector where
several share it."
  (let ((best nil))
    (dotimes (index (length sections))
      (let ((start (car (aref sections index))))
        (when (and (> start row-index)
                   (or (null best) (< start (car (aref sections best)))))
          (setq best index))))
    best))

(defun align-buffer--section-before (row-index sections)
  "Return the index of the section to move to from ROW-INDEX, going backwards.
The latest start below ROW-INDEX, with the same tie-break as forwards."
  (let ((best nil))
    (dotimes (index (length sections))
      (let ((start (car (aref sections index))))
        (when (and (< start row-index)
                   (or (null best) (> start (car (aref sections best)))))
          (setq best index))))
    best))


;;; Going there

(defun align-buffer--section-height (section)
  "Return how many rows SECTION covers."
  (1+ (- (cdr section) (car section))))

(defun align-buffer--centre-section (window section)
  "Scroll WINDOW so SECTION reads centred, with point on its first row."
  (let* ((height (window-body-height window))
         (top (max (align-buffer--effective-margin window)
                   (/ (- height (align-buffer--section-height section)) 2))))
    (recenter top)))

(defun align-buffer--goto-section (session index)
  "Put point on the first row of section INDEX of SESSION and centre it."
  (let* ((sections (align-buffer-session-sections session))
         (section (aref sections index))
         (position (align-buffer-row-beginning-position
                    session (car section) align-buffer--side)))
    (unless position
      (user-error "Section %d starts outside the plan" index))
    (goto-char position)
    (align-buffer--centre-section (selected-window) section)))


;;; Commands

(defun align-buffer-next-section (&optional count)
  "Move to the COUNTth next section."
  (interactive "p")
  (align-buffer--move-section (or count 1)))

(defun align-buffer-previous-section (&optional count)
  "Move to the COUNTth previous section."
  (interactive "p")
  (align-buffer--move-section (- (or count 1))))

(defun align-buffer--move-section (count)
  "Move COUNT sections forward, or backward when COUNT is negative."
  (let* ((session (or (align-buffer-current-session)
                      (user-error "Not in an align-buffer pane")))
         (sections (align-buffer-session-sections session)))
    (when (zerop (length sections))
      (user-error "Nothing to navigate"))
    (dotimes (_ (abs count))
      (let* ((row-index (align-buffer-row-at-point))
             (index (if (> count 0)
                        (align-buffer--section-after row-index sections)
                      (align-buffer--section-before row-index sections))))
        (unless index (user-error "No more sections"))
        (align-buffer--goto-section session index)))))

(defun align-buffer-visit-source ()
  "Go to the position in the source buffer that this row stands for.
On a padding row, the nearest row above that shows a source line."
  (interactive)
  (let* ((session (or (align-buffer-current-session)
                      (user-error "Not in an align-buffer pane")))
         (side align-buffer--side)
         (row-index (align-buffer-row-at-point))
         (offset (- (point) (line-beginning-position)))
         (cell (or (align-buffer-cell session row-index side)
                   (user-error "This pane has no rows")))
         (kind (align-buffer-cell-kind cell))
         (target (and (not (eq kind 'text))
                      (align-buffer--nearest-source-row
                       session row-index side))))
    (when (eq kind 'text)
      (user-error "This row is not a line of any buffer"))
    (unless target
      (user-error "Nothing on this side of the pane comes from a buffer"))
    (let ((source (align-buffer-source-position
                   session target side
                   (and (eq target row-index) offset))))
      (unless source
        (user-error "This row shows no source line"))
      (pop-to-buffer (car source))
      (goto-char (cdr source)))))

(defun align-buffer--nearest-source-row (session row-index side)
  "Return ROW-INDEX, or the nearest row above whose SIDE shows a source line."
  (cl-loop for candidate from row-index downto 0
           for cell = (align-buffer-cell session candidate side)
           when (eq (align-buffer-cell-kind cell) 'line)
           return candidate))

(provide 'align-buffer-navigation)

;;; align-buffer-navigation.el ends here
