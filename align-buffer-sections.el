;;; align-buffer-sections.el --- Move from one stretch of interest to the next -*- lexical-binding: t; -*-

;;; Commentary:

;; A section is a stretch of lines the consumer calls interesting: the lines
;; one hunk of a diff covers, a block that moved, whatever the layer above is
;; about.  Each one says where it begins and how many lines it has on each
;; side, so a side with none of them has a section sitting in front of a line
;; rather than on one.
;;
;; `align-buffer-next-section' and `align-buffer-previous-section' take the
;; reader from one to the next, in the pane they are standing in, and leave it
;; reading centred.  The pane facing them comes level the way it does after any
;; move of theirs.  Where their side has none of the section's lines and a pad
;; stands for them, the move walks into that pad, so a run the other side alone
;; has is read from inside it.
;;
;; Running out of sections is a hook, `align-buffer-section-boundary-functions',
;; so a consumer showing one file of several can move to the next one instead of
;; stopping.  On its own, navigation says there is no more and leaves the reader
;; where they are.

;;; Code:

(require 'cl-lib)
(require 'align-buffer-core)
(require 'align-buffer-sync)
(require 'align-buffer-pads)


;;; What the consumer marks

(defun align-buffer-check-sections (sections)
  "Signal unless every element of SECTIONS is a run of lines on each side."
  (dolist (section (append sections nil))
    (unless (and (eql (proper-list-p section) 4)
                 (cl-every #'integerp section)
                 (> (nth 0 section) 0) (>= (nth 1 section) 0)
                 (> (nth 2 section) 0) (>= (nth 3 section) 0))
      (error "Not a section: %S" section))))

(defun align-buffer-set-sections (session sections)
  "Have SESSION carry SECTIONS as the stretches to navigate between.

SECTIONS is a list or vector of (LEFT-LINE LEFT-COUNT RIGHT-LINE RIGHT-COUNT),
each giving the one-based line a stretch begins on and how many lines it has on
that side.  A count of zero says the side has none of them, and LINE is then
the line the missing ones belong in front of.  They need not be sorted."
  (align-buffer-check-sections sections)
  (setf (align-buffer-session-sections session) (append sections nil))
  session)

(defun align-buffer-section-line (section side)
  "Return the line SECTION begins on, on SIDE."
  (if (eq side 'left) (nth 0 section) (nth 2 section)))

(defun align-buffer-section-count (section side)
  "Return how many lines SECTION has on SIDE."
  (if (eq side 'left) (nth 1 section) (nth 3 section)))


;;; Where a section stands, and where the reader does

;; Both are a place, as (LINE . GAP): a line of the pane, or the boundary in
;; front of that line where the side has no line of its own to stand on.  So
;; the two sorts of section order against each other and against the reader
;; without a special case, and a run the other side alone has is a stop of its
;; own rather than a line the reader passes through.

(defun align-buffer--section-place (section side)
  "Return where SECTION stands on SIDE, as (LINE . GAP)."
  (cons (align-buffer-section-line section side)
        (zerop (align-buffer-section-count section side))))

(defun align-buffer--place-above-p (one other)
  "Return non-nil when place ONE comes above place OTHER.
The boundary in front of a line comes above the line itself."
  (or (< (car one) (car other))
      (and (= (car one) (car other)) (cdr one) (not (cdr other)))))

(defun align-buffer--section-at (place sections side)
  "Return the place of the section of SECTIONS whose SIDE covers PLACE."
  (unless (cdr place)
    (cl-loop for section in sections
             for first = (align-buffer-section-line section side)
             for count = (align-buffer-section-count section side)
             when (and (> count 0)
                       (<= first (car place))
                       (< (car place) (+ first count)))
             return (cons first nil))))

(defun align-buffer--reader-place (sections side)
  "Return where the reader stands in the current pane, as (LINE . GAP).

The place of the whole section they are standing in, where they are in one:
the next section is then the one after it, and a reader half way down a long
stretch is taken out of it rather than back to its first line.

A pad being walked is that pane's own picture of a section it has no lines
of, so the reader stands at the boundary its rows hang on."
  (let ((place (if align-buffer--walked
                   (cons (align-buffer-line-at
                          (current-buffer)
                          (align-buffer-pad-anchor
                           (car align-buffer--walked)))
                         t)
                 (cons (align-buffer-line-at (current-buffer)
                                             (line-beginning-position))
                       nil))))
    (or (align-buffer--section-at place sections side) place)))

(defun align-buffer--section-toward (place sections side forward)
  "Return the section of SECTIONS nearest PLACE on SIDE.
Below PLACE with FORWARD, above it without, and nil where SECTIONS has none
that way."
  (let ((best nil)
        (best-place nil))
    (dolist (section sections)
      (let ((candidate (align-buffer--section-place section side)))
        (when (if forward
                  (align-buffer--place-above-p place candidate)
                (align-buffer--place-above-p candidate place))
          (when (or (null best)
                    (if forward
                        (align-buffer--place-above-p candidate best-place)
                      (align-buffer--place-above-p best-place candidate)))
            (setq best section)
            (setq best-place candidate)))))
    best))


;;; Going there

(defun align-buffer--section-position (buffer line)
  "Return where LINE of BUFFER begins, or where its text ends past the last.
A run the other side alone has at the very end belongs past the last line, and
what stands for it there hangs at the end of the text."
  (let ((position (align-buffer-line-position buffer line)))
    (if (eql (align-buffer-line-at buffer position) line)
        position
      (with-current-buffer buffer
        (save-restriction (widen) (point-max))))))

(defun align-buffer--centre-pixels-of (window pixels)
  "Return how far below WINDOW's top a block PIXELS tall reads centred.
The scroll margin at least, so that what is centred is not left where
redisplay would scroll it away, and the top of the window for a block taller
than the window is.  On a row boundary, which is as near the middle as a
window can be scrolled."
  (let ((height (align-buffer--row-height window)))
    (* (max (align-buffer-effective-margin window)
            (/ (max 0 (- (window-body-height window t) pixels)) (* 2 height)))
       height)))

(defun align-buffer--centre-pixels (window rows)
  "Return how far below WINDOW's top a run of ROWS rows reads centred."
  (align-buffer--centre-pixels-of
   window (* rows (align-buffer--row-height window))))

(defun align-buffer--centre-target (window position rows)
  "Return where POSITION is levelled to for ROWS rows of it to read centred.
What hangs in front of POSITION is drawn above its own text, so the levelling
aims that much higher and the text lands on the centre.  Not above the
window's top, where what hangs is taller than the centre and the window is
held against it instead."
  (max 0 (- (align-buffer--centre-pixels window rows)
            (align-buffer--hanging-height window position))))

(defun align-buffer--centre-at (window position pixels)
  "Scroll WINDOW so that POSITION is drawn PIXELS below its top.

Two steps, because a start is a position and the rows an overlay hangs hold
none: recentring gets the window as near as a row boundary allows, which is
where there is something to measure at all, and the rest of the way is a
scroll through whatever is drawn there - a block of rows included, which no
start of its own could land inside."
  (with-selected-window window
    (recenter (/ pixels (align-buffer--row-height window)))
    (align-buffer--level-exactly window position pixels)))

(defun align-buffer--goto-section (session side section)
  "Put the reader on SECTION of SESSION's SIDE, reading centred.

Where the side has none of the section's lines and a pad stands for them, the
first of its rows is walked into instead: those rows are what this pane shows
for the lines, so they are what there is to read.  Without the mode that walks
them the rows are display nothing steps through, and the reader goes to the
line below the boundary as they would with no pad there at all."
  (let* ((buffer (align-buffer-buffer session side))
         (position (align-buffer--section-position
                    buffer (align-buffer-section-line section side)))
         (window (get-buffer-window buffer))
         (pad (and (zerop (align-buffer-section-count section side))
                   (buffer-local-value 'align-buffer-navigation-mode buffer)
                   (align-buffer-pad-at position buffer))))
    ;; Whatever the reader was reading before, they are leaving it.
    (align-buffer--walk-away)
    (with-current-buffer buffer
      (cond
       (pad
        (goto-char (align-buffer-pad-hangs pad))
        (when (window-live-p window)
          (set-window-point window (point))
          ;; The rows are the section as this pane shows it, so they are what
          ;; is centred.  Levelling is asked of the pad's anchor, the rows
          ;; holding no position to ask it of, and that anchor is drawn the
          ;; whole block of rows below where the block begins.
          (let ((block (align-buffer-pad-height window pad)))
            (align-buffer--centre-at
             window (align-buffer-pad-anchor pad)
             (+ (align-buffer--centre-pixels-of window block) block))))
        (align-buffer--walk-into pad 0 window t))
       (t
        (goto-char position)
        (when (window-live-p window)
          (set-window-point window (point))
          (align-buffer--centre-at
           window position
           (align-buffer--centre-target
            window position
            (align-buffer-section-count section side)))))))))


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

;;;###autoload
(defun align-buffer-next-section (&optional count)
  "Move to the COUNTth next section."
  (interactive "p")
  (align-buffer--move-section (or count 1)))

;;;###autoload
(defun align-buffer-previous-section (&optional count)
  "Move to the COUNTth previous section."
  (interactive "p")
  (align-buffer--move-section (- (or count 1))))

(defun align-buffer--move-section (count)
  "Move COUNT sections forward, or backward when COUNT is negative.
A session carrying no sections at all is a boundary like any other."
  (let* ((session (or (align-buffer-current-session)
                      (user-error "Not in an align-buffer pane")))
         (side (align-buffer-side))
         (sections (align-buffer-session-sections session))
         (step (if (> count 0) 1 -1))
         (remaining count))
    (if (null sections)
        (align-buffer--section-boundary session remaining "Nothing to navigate")
      (while (not (zerop remaining))
        (let ((section (align-buffer--section-toward
                        (align-buffer--reader-place sections side)
                        sections side (> step 0))))
          (cond
           (section
            (align-buffer--goto-section session side section)
            (cl-decf remaining step))
           (t
            ;; A handler may well have moved us to another session, where the
            ;; moves that are left mean nothing.
            (align-buffer--section-boundary session remaining
                                            "No more sections")
            (setq remaining 0))))))))

(provide 'align-buffer-sections)

;;; align-buffer-sections.el ends here
