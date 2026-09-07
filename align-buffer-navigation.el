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
;;
;; Running out of sections is a hook, `align-buffer-section-boundary-functions',
;; so a consumer showing one file of several can move to the next one instead of
;; stopping.  On its own, navigation says there is no more and leaves point
;; where it is.

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


;;; Running out of sections

(defvar align-buffer-section-boundary-functions nil
  "Functions run when section navigation has nowhere left to go.
Called with the session and how many moves are left to make, the one that
could not be made included: positive going forwards, negative going backwards.
The first to return non-nil has handled the boundary, and the command stops
there rather than spending the moves that are left.  When none does, the
command signals a `user-error'.

The panes are ordinary buffers, so a handler for one session alone is this hook
made buffer-local in that session's panes.")

(defun align-buffer--section-boundary (session remaining message)
  "Hand SESSION's boundary to the hook, or signal MESSAGE as a `user-error'.
REMAINING is the signed count of moves still to make."
  (or (run-hook-with-args-until-success
       'align-buffer-section-boundary-functions session remaining)
      (user-error "%s" message)))


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
  "Move COUNT sections forward, or backward when COUNT is negative.
A plan with no sections at all is a boundary like any other."
  (let* ((session (or (align-buffer-current-session)
                      (user-error "Not in an align-buffer pane")))
         (sections (align-buffer-session-sections session))
         (step (if (> count 0) 1 -1))
         (remaining count))
    (if (zerop (length sections))
        (align-buffer--section-boundary session remaining "Nothing to navigate")
      (while (not (zerop remaining))
        (let* ((row-index (align-buffer-row-at-point))
               (index (if (> step 0)
                          (align-buffer--section-after row-index sections)
                        (align-buffer--section-before row-index sections))))
          (cond
           (index
            (align-buffer--goto-section session index)
            (cl-decf remaining step))
           (t
            ;; A handler may well have moved us to another session, where the
            ;; moves that are left mean nothing.
            (align-buffer--section-boundary session remaining "No more sections")
            (setq remaining 0))))))))

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
