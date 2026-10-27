;;; align-buffer-pads.el --- Rows standing for lines a side has not got -*- lexical-binding: t; -*-

;;; Commentary:

;; A pad is a run of blank rows hanging above a line of one buffer, standing
;; for lines the facing buffer has there and this one has not.  It makes the
;; correspondence whole: a reader on those lines has a row to be level with
;; all the way down the run, so the two panes go on reading row for row where
;; a pair alone would leave them a run apart.
;;
;; A consumer puts the rows there with `align-buffer-pad-add' and takes them
;; away with `align-buffer-pad-clear'.  What they look like is the consumer's,
;; which hands in the face they are drawn in; what they behave like is here.
;;
;; `align-buffer-navigation-mode' is what makes them behave like the lines
;; they stand for: the ordinary motion and scroll commands walk them row by
;; row instead of stepping over the lot in one go, which is what they do with
;; display no position in the buffer falls inside.

;;; Code:

(require 'align-buffer-core)
(require 'align-buffer-sync)

(defvar-local align-buffer--pads nil
  "Non-nil while this buffer has pads of ours in it.")

(defvar-local align-buffer--walked nil
  "The pad being walked and the row the cursor is on, as (OVERLAY . ROW).")

(defvar-local align-buffer--walk-stepped nil
  "Non-nil when the command being run has stepped onto a row of a pad.
Read once the command is over and cleared there, which is what tells a cursor
another package moved afterwards from point the reader moved themselves.")

(defun align-buffer--pads-in-p (buffer)
  "Return non-nil when BUFFER has had pads of ours put in it."
  (and (buffer-live-p buffer)
       (buffer-local-value 'align-buffer--pads buffer)
       t))

(defun align-buffer--pads-level-exactly (buffer exactly)
  "Say whether the session BUFFER belongs to is to be levelled to the pixel.
A pad's rows fall between the row boundaries a placement lands on, so panes
with pads between them cannot read level without a lift."
  (when-let ((session (align-buffer-session-for buffer)))
    (setf (align-buffer-session-properties session)
          (plist-put (align-buffer-session-properties session)
                     :exact-alignment exactly))))


;;; Making a pad

(defun align-buffer--pad-row (face)
  "Return one blank row of a pad, faced FACE.
A space before the newline, so the row has a glyph of its own: a row drawn
from nothing at all can carry neither the cursor nor anything else, and
walking a pad is done by asking for the cursor on one of its rows."
  (propertize " \n" 'face face))

(defun align-buffer--pad-string (rows face opening)
  "Return the display of a pad of ROWS rows, faced FACE.

It opens with a newline faced OPENING, which ends the row of the line it
hangs after.  That is the face of the line itself: what paints a row to the
edge of the window is the face of whatever newline closes it, so a coloured
line needs its own colour here to be painted as far as it is anywhere else,
and an uncoloured one needs none and is left alone.

Each row after that closes with a coloured newline of its own.  The last
closes with the line's own newline instead, which the pad is drawn over so
that it is painted too."
  (concat (if opening (propertize "\n" 'face opening) "\n")
          (apply #'concat (make-list (1- rows) (align-buffer--pad-row face)))
          (propertize " " 'face face)))

(defun align-buffer--pad-run-rows (facing facing-line lines)
  "Return how a pad's rows stand against LINES lines of FACING, or nil.

As (ROWS . TEXT).  ROWS says which line each row of the pad stands for, there
being a row for every screen row the run of lines is drawn as over there: a
line that wraps is drawn as several and is given that many, and so is a line
something else hangs rows on.  TEXT says, for each line counting from
FACING-LINE, which row faces the line's own text, past the rows of whatever
hangs in front of it.

Nil where those lines are drawn nowhere yet, no window of theirs being up to
measure them in, and a pad then draws one row for each line."
  (let ((window (and (buffer-live-p facing) (get-buffer-window facing))))
    (when (and window facing-line (> lines 0))
      (let ((height (align-buffer--row-height window))
            (rows nil)
            (text (make-vector lines 0))
            (row 0))
        (dotimes (index lines)
          (let* ((line (+ facing-line index))
                 (position (align-buffer-line-position facing line))
                 (hanging (if position
                              (max 0 (round (align-buffer--hanging-height
                                             window position)
                                            height))
                            0))
                 (whole (if position
                            (max (1+ hanging)
                                 (round (align-buffer-line-display-height
                                         window position)
                                        height))
                          1)))
            (aset text index (+ row hanging))
            (dotimes (_ whole) (push line rows))
            (setq row (+ row whole))))
        (cons (vconcat (nreverse rows)) text)))))

(defun align-buffer-pad-facing-line (overlay row)
  "Return the line of the facing side ROW of OVERLAY's pad stands for."
  (let ((rows (car (overlay-get overlay 'align-buffer-pad-run)))
        (first (overlay-get overlay 'align-buffer-pad-facing-line)))
    (if (and rows (>= row 0) (< row (length rows)))
        (aref rows row)
      (and first (+ first row)))))

(defun align-buffer-pad-text-row (overlay line)
  "Return the row of OVERLAY's pad facing LINE's own text, or nil."
  (let ((text (cdr (overlay-get overlay 'align-buffer-pad-run)))
        (first (overlay-get overlay 'align-buffer-pad-facing-line)))
    (when first
      (let ((index (- line first)))
        (cond ((and text (>= index 0) (< index (length text)))
               (aref text index))
              ((and (null text) (>= index 0)
                    (< index (align-buffer-pad-rows overlay)))
               index))))))

(defun align-buffer-pad-anchor (overlay)
  "Return where the line OVERLAY's rows are drawn in front of begins.
What the rows are measured from: they are the display of the lines the other
side has, and they end where this line's own text begins."
  (overlay-end overlay))

(defun align-buffer-pad-hangs (overlay)
  "Return where point belongs while OVERLAY's rows are being read.
The place the rows are drawn at, which is the end of the line they hang
after: a row carries the cursor only while point is at that place, so this is
where point waits for as long as a row is being read, and the anchor is no
good for it."
  (overlay-start overlay))

(defun align-buffer-pad-add (buffer position rows &optional face facing
                                    facing-line)
  "Give BUFFER ROWS blank rows in front of POSITION, drawn in FACE.
Return the overlay they are drawn by.

The rows hang at the end of the line before POSITION, drawn in front of that
line's own newline, which is where the lines they stand for belong and what
makes them rows of that line's display.  So every line of the text keeps its
own line number beside its own text, and the rows carry none.  At the very
beginning of the buffer there is no line before to hang them on, and they go
in front of the first line instead.

FACING is the buffer that has those lines and FACING-LINE the first of them,
which is what lets a walk of the pad scroll the facing window alongside."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (save-excursion
        (goto-char position)
        (let* ((anchor (point))
               ;; The end of the line before, where the rows are drawn after
               ;; its text and in front of the newline closing its row.  An
               ;; empty line has its end at its own beginning, so rows hung
               ;; there would be drawn in front of that line rather than after
               ;; it, and the line would read a pad's height below its pair;
               ;; they go in front of the line they belong before instead,
               ;; which is where the first line of a buffer has them too.
               (hangs (if (bobp) anchor
                        (save-excursion
                          (forward-line -1)
                          (if (= (line-beginning-position) (line-end-position))
                              anchor
                            (line-end-position)))))
               ;; A row for every screen row the run is drawn as over there,
               ;; and which line each of them stands for.
               (run (align-buffer--pad-run-rows facing facing-line rows))
               (rows (if run (length (car run)) rows))
               ;; The colour of the line the rows hang after, read before
               ;; the pad is there to be read instead of it.  A hunk with
               ;; lines on both sides has its own faced already, and the
               ;; rows of a pad hanging off one of them must not swallow
               ;; the colour of its row.
               (opening (and (/= hangs anchor)
                             (get-char-property hangs 'face)))
               (overlay (make-overlay hangs anchor buffer)))
          (setq align-buffer--pads t)
          (align-buffer--pads-level-exactly buffer t)
          (overlay-put overlay 'align-buffer-pad-rows rows)
          (overlay-put overlay 'align-buffer-pad-run run)
          (overlay-put overlay 'align-buffer-pad-facing facing)
          (overlay-put overlay 'align-buffer-pad-facing-line facing-line)
          (overlay-put overlay 'align-buffer-pad-opening opening)
          (unless (= hangs anchor)
            ;; The newline the rows are drawn in front of closes the last of
            ;; them, so it is painted like the others, and above the colour
            ;; of the line it belongs to.
            (overlay-put overlay 'face face)
            (overlay-put overlay 'priority 5))
          (overlay-put overlay 'before-string
                       (if (= hangs anchor)
                           (apply #'concat
                                  (make-list rows
                                             (align-buffer--pad-row face)))
                         (align-buffer--pad-string rows face opening)))
          overlay)))))

(defun align-buffer-pad-clear (buffer)
  "Take every pad of ours out of BUFFER, leaving none of them walked."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq align-buffer--walked nil)
      (setq align-buffer--pads nil)
      (align-buffer--pads-level-exactly buffer nil)
      (save-restriction
        (widen)
        (dolist (overlay (overlays-in (point-min) (point-max)))
          (when (overlay-get overlay 'align-buffer-pad-rows)
            (delete-overlay overlay)))))))

;;; Walking a pad

;; A pad's rows are display and hold no position, so point cannot stand on
;; one.  What stands on one is the cursor: a row carrying a `cursor' property
;; is where redisplay draws it while point is at the position the pad hangs
;; on.  Walking a pad is therefore moving that property from row to row, with
;; point waiting at the line below, and scrolling the window when the row
;; being walked to would be off screen.

(defun align-buffer-pad-at (position &optional buffer)
  "Return the pad whose rows are drawn in front of POSITION of BUFFER.
BUFFER goes with POSITION, the current buffer by default: a position of the
facing side read in this side's buffer finds whatever happens to lie at that
number of characters, which is a pad of the wrong pane or none.

A pad hangs off the newline before POSITION, so it is looked for over that
newline rather than at POSITION itself."
  (with-current-buffer (or buffer (current-buffer))
    (seq-find (lambda (overlay)
                (and (overlay-get overlay 'align-buffer-pad-rows)
                     (= (align-buffer-pad-anchor overlay) position)))
              (overlays-in (max (point-min) (1- position)) position))))

(defun align-buffer-pad-rows (overlay)
  "Return how many rows OVERLAY's pad has."
  (or (overlay-get overlay 'align-buffer-pad-rows) 0))

(defun align-buffer-pad-row-height (window _overlay _row)
  "Return how tall a row of a pad is drawn in WINDOW, in pixels.
One row of the window: a pad draws a row for each line the other side has,
every one of them the same height."
  (align-buffer--row-height window))

(defun align-buffer--pad-above (window _overlay row)
  "Return the pixels of a pad WINDOW draws above its ROWth row.
The whole pad where ROW is the row after its last, which is what the line the
rows stand in front of is drawn below."
  (* row (align-buffer--row-height window)))

(defun align-buffer-pad-height (window overlay)
  "Return how tall the whole of OVERLAY's pad is drawn, in pixels."
  (align-buffer--pad-above window overlay (align-buffer-pad-rows overlay)))

(defun align-buffer--pad-show-cursor (overlay row)
  "Ask for the cursor on ROW of OVERLAY's pad, or on none when ROW is nil."
  (let* ((rows (align-buffer-pad-rows overlay))
         ;; A pad in front of the first line of the buffer has no newline of
         ;; the line before to hang after, so its rows are drawn without the
         ;; one that opens the others, and every index into it is one lower.
         (hanging (/= (overlay-start overlay)
                      (align-buffer-pad-anchor overlay)))
         (first (if hanging 1 0))
         (face (get-text-property first 'face
                                  (overlay-get overlay 'before-string)))
         (string (if hanging
                     (align-buffer--pad-string
                      rows face (overlay-get overlay
                                             'align-buffer-pad-opening))
                   (apply #'concat
                          (make-list rows (align-buffer--pad-row face))))))
    (when row
      ;; The space of that row, which is the glyph the row is drawn from.
      (let ((at (+ first (* row 2))))
        (put-text-property at (1+ at) 'cursor t string)))
    (overlay-put overlay 'before-string string)))

(defun align-buffer--pad-uncounted (window from to)
  "Return the height of the pad rows WINDOW draws below FROM without counting.

A span can begin inside the one character a pad hangs on: that character is
the newline the rows are drawn in front of, and `recenter' leaves a window
start there, there being no row boundary inside the rows for it to take
instead.  The rows are then drawn from the very top of the window, and a span
measured from there is measured as none of them.

The whole pad counts, since all of it is drawn above the line it hangs in
front of: a span reaching that line, or anywhere below it, has every row of
the pad between its ends."
  (let ((pad (with-current-buffer (window-buffer window)
               (save-restriction
                 (widen)
                 ;; The pad hanging at FROM, which is either the one in front
                 ;; of the line after it or, at the very beginning of the
                 ;; buffer, the one in front of FROM's own line: a pad there
                 ;; has no line before it to hang after, so it hangs where it
                 ;; is drawn.
                 (let ((here (align-buffer-pad-at from))
                       (next (and (< from (point-max))
                                  (align-buffer-pad-at (1+ from)))))
                   (cond
                    ((and here (= (align-buffer-pad-hangs here) from)) here)
                    ((and next (= (align-buffer-pad-hangs next) from))
                     next)))))))
    (if (and pad (>= to (align-buffer-pad-anchor pad)))
        (align-buffer-pad-height window pad)
      0)))

(setq align-buffer-uncounted-height-function #'align-buffer--pad-uncounted)

(defun align-buffer--offset-in (window position)
  "Return how far below WINDOW's top POSITION is drawn, negative when above it.
A span is measured whichever way round it is given, so a position above the
start has to be measured back to the start and turned about; the window's lift
belongs to the start's own line and so is added to that span rather than taken
off it.

What hangs at POSITION is drawn above POSITION's own text and so lies outside
the span back to the start, which counts it all the same; measuring down to
POSITION counts it on the other edge, where it belongs.  So turning the span
about puts it back, and a line drawn as eighty rows is not reported eighty
rows further off than it is."
  (let ((start (window-start window)))
    (if (>= position start)
        (+ (cdr (window-text-pixel-size window start position))
           (align-buffer--uncounted-height window start position))
      (+ (align-buffer--hanging-height window position)
         (- (cdr (window-text-pixel-size window start start))
            (+ (cdr (window-text-pixel-size window position start))
               (window-vscroll window t)))))))

(defun align-buffer--pad-top (window overlay)
  "Return how far below WINDOW's top OVERLAY's first pad row is drawn."
  (- (align-buffer--offset-in window (align-buffer-pad-anchor overlay))
     (align-buffer-pad-height window overlay)))

(defun align-buffer--pad-reveal (window overlay row)
  "Scroll WINDOW so that ROW of OVERLAY's pad is clear of its edges."
  (let* ((height (align-buffer-pad-row-height window overlay row))
         (band (align-buffer--band window))
         (top (+ (align-buffer--pad-top window overlay)
                 (align-buffer--pad-above window overlay row))))
    (cond ((< top (car band))
           (align-buffer-scroll window (- top (car band))))
          ((> (+ top height) (cdr band))
           (align-buffer-scroll window (- (+ top height) (cdr band)))))))

(defun align-buffer--walk-into (overlay row &optional window reveal)
  "Put the cursor on ROW of OVERLAY's pad and keep both panes showing it.
With REVEAL, WINDOW is scrolled until that row is clear of its edges, which is
what a step of a line asks for; without it the window stays where it is, which
is what a scroll of the reader's asks for."
  (let ((window (or window (selected-window))))
    (align-buffer--pad-show-cursor overlay row)
    (setq align-buffer--walked (and row (cons overlay row)))
    (setq align-buffer--walk-stepped (and row t))
    ;; Both windows are ours while a pad is walked: the pairs say nothing
    ;; about a row that stands for a line this side has not got, so the sync
    ;; would pull them back to the pair below the pad.
    (setq align-buffer-inhibit-sync (and row t))
    ;; Point stays where the rows are drawn, which a row needs it to do to
    ;; carry the cursor at all, and that place is the end of the line they
    ;; hang after.  It is not where the step was going: the rows stand for
    ;; lines of the other side, and none of them is a position here.
    (when row
      (goto-char (align-buffer-pad-hangs overlay))
      (when (eq (window-buffer window) (current-buffer))
        (set-window-point window (point))))
    ;; The line point waits on carries every one of the rows, so the cursor
    ;; walks away from point down the window while point stays where it is,
    ;; and a view that follows the cursor leaves point in the margin
    ;; redisplay scrolls for, or off the screen altogether.
    (if row
        (align-buffer-hold-margin (current-buffer))
      (align-buffer-release-margin (current-buffer)))
    (when row
      (when reveal (align-buffer--pad-reveal window overlay row))
      (align-buffer--pad-face-row window overlay row))))

(defun align-buffer--pad-face-row (window overlay row)
  "Scroll the facing window so its own line faces ROW of OVERLAY's pad.
The pad stands for lines the facing side has, so walking its rows walks those
lines, and the two panes go on reading level."
  (let* ((facing (overlay-get overlay 'align-buffer-pad-facing))
         (line (align-buffer-pad-facing-line overlay row))
         (other (and (buffer-live-p facing) (get-buffer-window facing))))
    (when (and other line)
      (let* (;; The rows of the line's own display this row is past: a line
             ;; drawn as several rows is faced by several, and the one being
             ;; read is that far down the line rather than at its text.
             (within (* (- row (or (align-buffer-pad-text-row overlay line)
                                   row))
                        (align-buffer--row-height window)))
             (wanted (- (+ (align-buffer--pad-top window overlay)
                           (align-buffer--pad-above window overlay row))
                        within))
             (position (align-buffer-line-position facing line)))
        ;; Scrolled by what it is out by, again and again until it is out by
        ;; nothing: one measurement across a long stretch of a buffer can be
        ;; wide of the mark, and a pass that lands on the wrong row leaves the
        ;; next one a short span to measure, where it is not.  Bounded, since
        ;; a row the window cannot put there at all would otherwise be chased
        ;; for ever.
        (let ((tries 0)
              (out (- (align-buffer--offset-in other position) wanted)))
          (while (and (not (zerop out)) (< tries 4))
            (align-buffer-scroll other out)
            (setq tries (1+ tries))
            (setq out (- (align-buffer--offset-in other position) wanted))))
        ;; And that line is where the facing pane is reading: a window is
        ;; scrolled by redisplay to show its own point, so a point left behind
        ;; on a line the lift has pushed off screen pulls the pane back.
        (set-window-point other position)
        ;; Noted as ours, or the next scroll hook of that pane reads our own
        ;; scroll as a reader's: it would take the lead the moment the walk
        ;; ends and place the reader's pane from where we left it.
        (align-buffer--note-placed other (window-start other)
                                   (window-point other))))))

(defun align-buffer--walk-away ()
  "Give the cursor back to the text, leaving no pad walked.

Both of the session's buffers, not merely this one: the walk is remembered
where the pad is, and a command run in the facing pane - `other-window', say -
must end it just the same, or the windows are left held for ever."
  (let* ((session (align-buffer-current-session))
         (buffers (if session
                      (list (align-buffer-buffer session 'left)
                            (align-buffer-buffer session 'right))
                    (list (current-buffer)))))
    (dolist (buffer buffers)
      (when (buffer-live-p buffer)
        (align-buffer-release-margin buffer)
        (with-current-buffer buffer
          (when align-buffer--walked
            (let ((overlay (car align-buffer--walked)))
              (when (overlay-buffer overlay)
                (align-buffer--pad-show-cursor overlay nil)))
            (setq align-buffer--walked nil))))))
  ;; The pairs can have the windows back.
  (setq align-buffer-inhibit-sync nil))

(defcustom align-buffer-wheel-rows 3
  "How many screen rows one notch of the wheel scrolls in a session."
  :type 'integer
  :group 'align-buffer)

(defvar align-buffer--scrolled-window nil
  "The window a scroll of ours has just moved, or nil.
Set where the scroll is carried out and read once by the walk check, which is
how a scroll of the view is told apart from a command that moved point without
either of them having to be named.")

(defun align-buffer--event-window (event)
  "Return the window EVENT points at, or the selected one when it points nowhere.
An event bound to a key carries no position, and a wheel one sent as plain
input is such an event."
  (or (and (consp event)
           (let ((window (posn-window (event-start event))))
             (and (windowp window) window)))
      (selected-window)))

(defun align-buffer-wheel-up (&optional event)
  "Scroll the window EVENT points at nearer the end of the buffer."
  (interactive (list last-input-event))
  (let ((window (align-buffer--event-window event)))
    (setq align-buffer--scrolled-window window)
    (align-buffer-scroll-rows align-buffer-wheel-rows window)
    (align-buffer--settle-scroll window)))

(defun align-buffer-wheel-down (&optional event)
  "Scroll the window EVENT points at nearer the beginning of the buffer."
  (interactive (list last-input-event))
  (let ((window (align-buffer--event-window event)))
    (setq align-buffer--scrolled-window window)
    (align-buffer-scroll-rows (- align-buffer-wheel-rows) window)
    (align-buffer--settle-scroll window)))

(defvar align-buffer-navigation-map
  (let ((map (make-sparse-keymap)))
    (define-key map [wheel-down] #'align-buffer-wheel-up)
    (define-key map [wheel-up] #'align-buffer-wheel-down)
    map)
  "What a session's buffers use for the events no command of Emacs's carries.")

(defun align-buffer--pad-showing (window pixels)
  "Return the pad whose rows WINDOW draws PIXELS below its top, or nil."
  (with-current-buffer (window-buffer window)
    (let* ((position (align-buffer--line-below window pixels))
           (pad (and position (align-buffer-pad-at position))))
      ;; A pad's rows come before the line's own text, so PIXELS falls inside
      ;; them exactly where that text is drawn further down than PIXELS.
      (and pad (> (align-buffer--offset-in window position) pixels) pad))))

(define-minor-mode align-buffer-navigation-mode
  "Minor mode moving through a pad's rows as though they were lines."
  :lighter nil
  :keymap align-buffer-navigation-map
  :group 'align-buffer
  (if align-buffer-navigation-mode
      (progn
        ;; Before align-buffer's own hook: the walk decides which of the two
        ;; panes are ours, and the sync has to read that decision rather than
        ;; place them first and be corrected.
        (add-hook 'post-command-hook #'align-buffer--walk-check nil t)
        ;; Last of all, so that point is where the row being read is drawn
        ;; whatever else in the hook has moved it.
        (add-hook 'post-command-hook #'align-buffer--walk-hold-point 100 t)
        ;; Where it beats the other minor modes rather than taking its turn
        ;; among them: `pixel-scroll-precision-mode' binds the wheel too, and
        ;; its notches scroll a window by pixels of its own without a word to
        ;; the pane facing it.
        (setq-local minor-mode-overriding-map-alist
                    (cons (cons 'align-buffer-navigation-mode
                                align-buffer-navigation-map)
                          (assq-delete-all 'align-buffer-navigation-mode
                                           minor-mode-overriding-map-alist))))
    (align-buffer--walk-away)
    (remove-hook 'post-command-hook #'align-buffer--walk-check t)
    (remove-hook 'post-command-hook #'align-buffer--walk-hold-point t)
    (setq-local minor-mode-overriding-map-alist
                (assq-delete-all 'align-buffer-navigation-mode
                                 minor-mode-overriding-map-alist))))

(defun align-buffer--step-down (original rest)
  "Take one step down, through a pad's rows where one is on the way.
Return non-nil when the reader moved, which is what ORIGINAL with REST answers
where the step is left to it."
  (pcase align-buffer--walked
    (`(,overlay . ,row)
     (if (< (1+ row) (align-buffer-pad-rows overlay))
         (progn (align-buffer--walk-into overlay (1+ row) nil t) t)
       ;; Out at the bottom of the rows, onto the line they are drawn in front
       ;; of.  Point is waiting on the line they hang after, so the step down
       ;; is the one the reader asked for and the column is kept by it.
       (align-buffer--walk-away)
       (apply original 1 rest)))
    (_
     (let* ((next (save-excursion (forward-line 1) (point)))
            (pad (align-buffer-pad-at next)))
       (cond
        ((and pad (> (align-buffer-pad-rows pad) 0))
         (align-buffer--walk-into pad 0 nil t)
         t)
        (t (apply original 1 rest)))))))

(defun align-buffer--step-up (original rest)
  "Take one step up, through a pad's rows where one is on the way.
Return non-nil when the reader moved, which is what ORIGINAL with REST answers
where the step is left to it."
  (pcase align-buffer--walked
    (`(,overlay . ,row)
     (if (> row 0)
         (progn (align-buffer--walk-into overlay (1- row) nil t) t)
       ;; Out at the top of the rows, onto the text of the line they hang
       ;; after.  Onto its beginning rather than by a step of `line-move's,
       ;; which counts the rows as a line of their own where they are drawn
       ;; and not at all where they are not.  The step has to be taken all
       ;; the same: the place point waits is the rows, and a reader left
       ;; standing there spends the press after this one getting off them.
       (align-buffer--walk-away)
       (goto-char (align-buffer-pad-hangs overlay))
       (beginning-of-line)
       t))
    (_
     (let ((pad (align-buffer-pad-at (line-beginning-position))))
       (cond
        ((and pad (> (align-buffer-pad-rows pad) 0))
         (align-buffer--walk-into pad (1- (align-buffer-pad-rows pad)) nil t)
         t)
        (t (apply original -1 rest)))))))

(defun align-buffer--pad-measuring-p (noerror)
  "Return non-nil when this `line-move' is measuring rather than moving.

`move-end-of-line' and `move-beginning-of-line' step a line to find where the
line they are on ends, inside `save-excursion' and with the goal column held
at nothing, and they ask not to be told when there is nowhere to go.  Nobody
has asked to move: a step onto a pad's rows from there would move the cursor
and start a walk of a line the reader is not on, and the line a command built
on it goes on to measure would be the wrong one.

Told apart by how `line-move' is called rather than by who called it, since
what a reader presses for a line reaches us under any name at all: evil's own
sets `this-command' to `next-line' for both."
  (and noerror (eql goal-column 0)))

(defun align-buffer--pad-line-move (original count &rest rest)
  "Move COUNT lines, walking the rows of the pads on the way.

Caught here rather than under the names of the commands that move a line:
`next-line', `previous-line' and every third-party motion built on them come
through `line-move', so one interception covers them all.  A buffer with no
session of ours moves as it always does."
  (if (or (not align-buffer-navigation-mode) (zerop count)
          (align-buffer--pad-measuring-p (car rest)))
      (apply original count rest)
    ;; Answering whether the reader moved, the way `line-move' does: what is
    ;; built on it reads that answer, and `move-end-of-line' comes back over
    ;; the newline it stepped past only when the step is said to have been
    ;; taken.
    (let ((moved t))
      (dotimes (_ (abs count))
        (when moved
          (setq moved (if (> count 0)
                          (align-buffer--step-down original rest)
                        (align-buffer--step-up original rest)))))
      moved)))

(advice-add 'line-move :around #'align-buffer--pad-line-move)

(defun align-buffer--rows-asked (arg window)
  "Return how many rows ARG asks WINDOW to be scrolled by.
A windowful less the context lines where ARG says nothing, which is what the
page commands mean by no argument."
  (let ((page (max 1 (- (window-body-height window)
                        next-screen-context-lines))))
    (cond ((null arg) page)
          ((eq arg '-) (- page))
          (t (prefix-numeric-value arg)))))

(defun align-buffer--pad-scroll (original arg down)
  "Scroll ARG rows through what the lines are drawn as, DOWN for nearer the end.
Caught at `scroll-up' and `scroll-down' for the reason a line move is caught
at `line-move': every scroll command of Emacs's and of a package's comes
through one of the two, so the rows a pad adds are scrolled through rather
than jumped over whichever command the reader has."
  (if (not align-buffer-navigation-mode)
      (funcall original arg)
    (let* ((window (selected-window))
           (start (window-start window))
           (lift (window-vscroll window t)))
      (setq align-buffer--scrolled-window window)
      (align-buffer-scroll-rows
       (let ((rows (align-buffer--rows-asked arg window)))
         (if down rows (- rows)))
       window)
      (align-buffer--settle-scroll window)
      ;; Signalled where the view did not move at all, as the commands we
      ;; stand in for do: a reader at the end of the buffer is told so, and
      ;; the commands built on these two have the end of their own work to
      ;; do - `evil-scroll-down' moves point the rest of the way itself.  A
      ;; scroll with somewhere to go, if not as far as it was asked, goes
      ;; there and says nothing.
      (when (and (= (window-start window) start)
                 (= (window-vscroll window t) lift))
        (signal (if down 'end-of-buffer 'beginning-of-buffer) nil))
      nil)))

(defun align-buffer--pad-scroll-up (original &optional arg)
  "Scroll nearer the end of the buffer, through what the lines are drawn as."
  (align-buffer--pad-scroll original arg t))

(defun align-buffer--pad-scroll-down (original &optional arg)
  "Scroll nearer the beginning of the buffer, through what the lines are drawn
as."
  (align-buffer--pad-scroll original arg nil))

(defun align-buffer--pad-scroll-other (original arg down)
  "Page the pair where the other window is the facing pane of a session.

The two panes of a session are one view, so there is no second window to
scroll on its own: asking for one pages both, with the pane the reader is in
leading as it does for every other scroll.  Any other window is scrolled as
it always was."
  (let ((other (and align-buffer-navigation-mode
                    ;; Errors demoted because it signals where there is no
                    ;; other window at all, which is a frame holding one pane
                    ;; and nothing of ours to scroll.
                    (ignore-errors (other-window-for-scrolling)))))
    (if (not (and (window-live-p other)
                  (buffer-local-value 'align-buffer-navigation-mode
                                      (window-buffer other))))
        (funcall original arg)
      (align-buffer--pad-scroll original arg down))))

(defun align-buffer--pad-scroll-other-up (original &optional arg)
  "Scroll the other window nearer the end of its buffer."
  (align-buffer--pad-scroll-other original arg t))

(defun align-buffer--pad-scroll-other-down (original &optional arg)
  "Scroll the other window nearer the beginning of its buffer."
  (align-buffer--pad-scroll-other original arg nil))

(advice-add 'scroll-other-window :around #'align-buffer--pad-scroll-other-up)
(advice-add 'scroll-other-window-down :around
            #'align-buffer--pad-scroll-other-down)
(advice-add 'scroll-up :around #'align-buffer--pad-scroll-up)
(advice-add 'scroll-down :around #'align-buffer--pad-scroll-down)

(defun align-buffer--click-row (window event)
  "Return the row of a pad in WINDOW that EVENT was sent at, as (PAD . ROW).
Nil where it landed on the text.  A click carries no position inside a pad,
every one of its rows hanging at the one place, so the row is the one drawn
where the pointer was."
  (let* ((position (event-end event))
         (y (and (windowp (posn-window position))
                 (cdr (posn-x-y position)))))
    (when y
      ;; Measured from the top of the text, which is what a click reports
      ;; from the top of the window: anything above the text is the window's
      ;; own and takes up none of what the pads are drawn on.
      (let* ((text (- y (window-tab-line-height window)
                      (window-header-line-height window)))
             (pad (and (>= text 0) (align-buffer--pad-showing window text))))
        (when pad
          (cons pad (align-buffer--pad-row-at window pad text)))))))

(defun align-buffer--pad-mouse-set-point (original event &rest rest)
  "Put the cursor on the row of a pad EVENT was sent at.

Caught at `mouse-set-point' for the reason a line move is caught at
`line-move': every command that puts point where the pointer is comes through
it, so a click lands on the row the reader clicked whichever command carried
it."
  (prog1 (apply original event rest)
    (when align-buffer-navigation-mode
      (let* ((window (align-buffer--event-window event))
             (clicked (and (eq (window-buffer window) (current-buffer))
                           (align-buffer--click-row window event))))
        (when clicked
          (goto-char (align-buffer-pad-hangs (car clicked)))
          (set-window-point window (point))
          (align-buffer--walk-into (car clicked) (cdr clicked) window))))))

(advice-add 'mouse-set-point :around #'align-buffer--pad-mouse-set-point)

(defun align-buffer--pad-point-for-scroll (window &optional pixels)
  "Return the line a pad whose rows WINDOW is showing hangs on, or nil.

A row carries the cursor only while point is on the line its pad hangs on, so
that line is where point belongs for as long as the reader is reading those
rows: the row being walked while it is still drawn, and otherwise the pad
drawn where point itself would have to go.

PIXELS is where the cursor is to stay, for a scroll keeping it on its screen
row; the pad to look for is then the one drawn there rather than the one
drawn where the band begins, and a row of it is where the cursor is going
whether a row was being walked before the scroll or not."
  (when (window-live-p window)
    (with-current-buffer (window-buffer window)
      (let ((walked align-buffer--walked))
        (cond
         (pixels
          (let ((pad (align-buffer--pad-showing window pixels)))
            (and pad (align-buffer-pad-hangs pad))))
         ((and walked (align-buffer--pad-row-shown-p window (car walked)
                                                     (cdr walked)))
          (align-buffer-pad-hangs (car walked)))
         ((align-buffer--pad-showing window (car (align-buffer--band window)))
          (align-buffer-pad-hangs
           (align-buffer--pad-showing window
                                      (car (align-buffer--band window))))))))))

(setq align-buffer-point-for-scroll-function
      #'align-buffer--pad-point-for-scroll)

(defun align-buffer--pad-cursor-pixels (window)
  "Return how far below WINDOW's top a walked row draws the cursor, or nil.
Nil where no row of a pad is being walked, which leaves the cursor to be
measured from point as it is anywhere else."
  (when (window-live-p window)
    (let ((walked (buffer-local-value 'align-buffer--walked
                                      (window-buffer window))))
      (when (and walked (overlay-buffer (car walked)))
        (+ (align-buffer--pad-top window (car walked))
           (* (cdr walked) (align-buffer--row-height window)))))))

(setq align-buffer-cursor-pixels-function #'align-buffer--pad-cursor-pixels)

(defun align-buffer--pad-at-top (window)
  "Return the pad whose rows the top of WINDOW falls inside, or nil.

Two pads can be drawn across the top: the one hanging after the start's own
line, which stands in front of the line below it, and - at the very beginning
of the buffer, where there is no line before to hang after - the one in front
of the start's own line.  Whichever of them is drawn from above the top and
reaches past it is the one the top falls inside.

A window begins inside the rows by being lifted through them, a start being a
position and the rows holding none."
  (with-current-buffer (window-buffer window)
    (save-restriction
      (widen)
      (let* ((start (window-start window))
             (nearby (delq nil
                           (list (align-buffer-pad-at start)
                                 (when-let ((next (align-buffer--line-after
                                                   start)))
                                   (align-buffer-pad-at next))))))
        (seq-find (lambda (pad)
                    (let ((top (align-buffer--pad-top window pad)))
                      (and (<= top 0)
                           (> (+ top (align-buffer-pad-height window pad))
                              0))))
                  nearby)))))

(defun align-buffer--facing-pad (session side line &optional window)
  "Return the pad standing for LINE on the side facing SIDE, as (PAD . ROW).

Nil unless LINE is one of a run the other side has not got.  The pads make
the correspondence whole: a run of lines one side alone has is drawn over
there as the rows of a pad, a row for every screen row the run is drawn as,
so a reader on the real lines has a row to be level with all the way down the
run.  The pad hangs on the line after the run's own pair, which is how it is
found.

Given WINDOW, the row is the one standing for the screen row the reader is on
rather than for the beginning of their line: a line of theirs that wraps is
read a row at a time and each of those rows has a row of the pad of its own."
  (let ((anchor (align-buffer-anchor session side line)))
    (when (and anchor (> line (car anchor)))
      (let* ((facing (align-buffer-buffer session
                                          (align-buffer-other-side side)))
             (position (align-buffer-line-position facing (1+ (cdr anchor))))
             (pad (and position (align-buffer-pad-at position facing)))
             ;; The row facing LINE's own text, and on from there by the
             ;; rows of their own line the reader has already walked.
             (text (and pad (align-buffer-pad-text-row pad line)))
             (into (if (and text window)
                       (max 0 (round (- (align-buffer--point-row-top window)
                                        (align-buffer--offset-in
                                         window
                                         (align-buffer-line-position
                                          (window-buffer window) line)))
                                     (align-buffer--row-height window)))
                     0))
             (row (and text (+ text into))))
        (when (and pad row
                   (< row (align-buffer-pad-rows pad))
                   (eq (overlay-get pad 'align-buffer-pad-facing)
                       (align-buffer-buffer session side)))
          (cons pad row))))))

(defun align-buffer--face-facing-row (window pad row)
  "Scroll PAD's own window so ROW faces the line point is on in WINDOW.

The mirror of levelling a walked row: there the reader is on the pad's rows
and the real lines are over the way, here the reader is on the real lines and
the rows are.  The cursor goes on the row for the same reason it does there,
a window being scrolled by redisplay to show its own point."
  (let ((other (and (overlay-buffer pad)
                    (get-buffer-window (overlay-buffer pad)))))
    (when (window-live-p other)
      (let* ((wanted (align-buffer--point-row-top window))
             (drawn (lambda ()
                      (+ (align-buffer--pad-top other pad)
                         (align-buffer--pad-above other pad row))))
             (tries 0)
             (out (- (funcall drawn) wanted)))
        (while (and (not (zerop out)) (< tries 4))
          (align-buffer-scroll other out)
          (setq tries (1+ tries))
          (setq out (- (funcall drawn) wanted)))
        (set-window-point other (align-buffer-pad-hangs pad))
        (align-buffer--pad-show-cursor pad row)
        (with-current-buffer (overlay-buffer pad)
          (setq align-buffer--walked (cons pad row)))
        ;; That pane now holds point on the pad's own line too, for the same
        ;; reason and with the same margin to keep out of redisplay's way.
        (align-buffer-hold-margin (overlay-buffer pad))
        (align-buffer--note-placed other (window-start other)
                                   (window-point other))))))

(defun align-buffer--point-row-top (window)
  "Return how far below WINDOW's top the row its point is on begins.

From where that row begins, not from point: a span to a position part way
along a row is measured as far as the bottom of that row, which reads as the
row below it and leaves everything measured against it a row out."
  (align-buffer--offset-in
   window (align-buffer--row-start window (window-point window))))

(defun align-buffer--pad-inside-p (window)
  "Return non-nil when point sits inside what WINDOW is showing at all.
A point above the top or further below than a windowful is one the window has
yet to be brought to, and the shortest scroll to it is not what a reader
wants: that is a placement, and the pairs make it."
  (let ((top (align-buffer--point-row-top window))
        (bottom (window-body-height window t)))
    (and (> top (- bottom)) (< top (* 2 bottom)))))

(defun align-buffer--keep-point-in-band (window)
  "Scroll WINDOW so the line point is on is drawn clear of its edges.

Ours to do while the top of the window is inside a pad.  Stepping back the
scroll margin from a line just below a pad taller than the window lands inside
the pad's rows, which redisplay cannot start a window on and still show point;
what it does instead is start the window on point's own line, and then do the
same again at every press, holding the cursor on the first row.  Scrolling by
the rows the line is short of the band keeps the line where a reader expects
it and leaves redisplay nothing to correct."
  ;; Point somewhere this view cannot show is point redisplay is about to
  ;; scroll the window to, and the lift belongs to the view being left: kept
  ;; across that scroll, it draws the line point lands on above the top of
  ;; the window, where there is no cursor to be seen.  So it goes, and
  ;; redisplay scrolls a window with nothing held back.
  (unless (align-buffer--pad-inside-p window)
    (align-buffer-forget-vscroll window))
  ;; Nothing to do at all while a row of a pad is being read: the cursor is
  ;; then drawn on the row and point waits on the line the rows hang after,
  ;; which the view is free to leave above its top.  Pulling the view back to
  ;; point there takes away the very scroll that brought the row into sight,
  ;; and the row then reads as one the reader has moved away from, so the
  ;; walk is given up and the press is spent on nothing.  This is the pull
  ;; `align-buffer-hold-margin' holds off redisplay for, and it is ours.
  ;;
  ;; The walk's own end asks for the pull instead, there being point to show
  ;; again from there on.
  (when (and (align-buffer--pad-inside-p window)
             (not (buffer-local-value 'align-buffer--walked
                                      (window-buffer window))))
    (let* ((height (align-buffer--row-height window))
           (band (align-buffer--band window))
           (top (align-buffer--point-row-top window)))
      (cond ((< top (car band))
             (align-buffer-scroll window (- top (car band))))
            ((> (+ top height) (cdr band))
             (align-buffer-scroll window (- (+ top height) (cdr band))))))))

(defun align-buffer--pad-row-at (window overlay pixels)
  "Return the row of OVERLAY's pad WINDOW draws PIXELS below its top.
The nearest row where the pad has none there, so a caller measuring against
an edge of the window always has a row to hold on to.

Walked down the rows rather than divided by one height, the rows standing for
lines of the facing side and being as tall as those lines are drawn."
  (let* ((rows (align-buffer-pad-rows overlay))
         (want (- pixels (align-buffer--pad-top window overlay)))
         (row 0)
         (below (align-buffer-pad-row-height window overlay 0)))
    (while (and (< row (1- rows)) (>= want below))
      (setq row (1+ row))
      (setq below (+ below (align-buffer-pad-row-height window overlay row))))
    (max 0 (min (1- rows) row))))

(defun align-buffer--pad-row-in-band (window overlay row)
  "Return ROW of OVERLAY's pad, held inside the rows WINDOW shows clear.
Nil when the pad has no row there at all.

What keeps a scroll honest: the row being read stays where it is on screen
while the view moves under it, and moves only when the view has scrolled past
it.  Clear of the margins as well, since a row against an edge is one the
facing pane cannot be brought level with, its own margin being in the way."
  (let* ((band (align-buffer--band window))
         (top (align-buffer--pad-top window overlay))
         (rows (align-buffer-pad-rows overlay))
         (low nil)
         (high nil))
    ;; Walked, since a row is only as tall as the line it stands for: the
    ;; first row drawn wholly below the top margin, and the last drawn wholly
    ;; above the bottom one.
    (dotimes (index rows)
      (let* ((above (+ top (align-buffer--pad-above window overlay index)))
             (below (+ above (align-buffer-pad-row-height window overlay
                                                           index))))
        (when (and (null low) (>= above (car band))) (setq low index))
        (when (<= below (cdr band)) (setq high index))))
    (and low high (<= low high) (min high (max low row)))))

(defun align-buffer--pad-row-shown-p (window overlay row)
  "Return non-nil when ROW of OVERLAY's pad is drawn inside WINDOW."
  (and (>= (align-buffer-pad-hangs overlay) (window-start window))
       (let* ((height (align-buffer-pad-row-height window overlay row))
              (top (+ (align-buffer--pad-top window overlay)
                      (align-buffer--pad-above window overlay row))))
         (and (>= top 0)
              (<= (+ top height) (window-body-height window t))))))

(defun align-buffer--show-placed-cursor (_session window _side)
  "Put the cursor on a pad row when a placement has left point off screen.

A window is scrolled by redisplay to show its own point, so one placed with
its point on the line a pad hangs on cannot be held deep inside that pad: the
line's text is then below everything shown.  A row of the pad can hold the
cursor, and holding it is what lets the window stay where it was put."
  (when (and (window-live-p window)
             (align-buffer--pads-in-p (window-buffer window)))
    (with-current-buffer (window-buffer window)
      (let ((offset (align-buffer--point-row-top window))
            (height (window-body-height window t))
            (row-height (align-buffer--row-height window)))
        (when (or (< offset 0) (> (+ offset row-height) height))
          ;; The pad point's own line carries, first: a line hanging a pad
          ;; taller than the window has its text below everything shown, and
          ;; the pad is what is drawn where the line belongs.  Failing that
          ;; the one the top of the window is inside, which is a window placed
          ;; further into a pad than its line.
          (let ((pad (or (align-buffer-pad-at (window-point window))
                         (align-buffer--pad-at-top window))))
            (when pad
              (let ((row (align-buffer--pad-row-in-band window pad 0)))
                (when row
                  (set-window-point window (align-buffer-pad-hangs pad))
                  (align-buffer--pad-show-cursor pad row)
                  (setq align-buffer--walked (cons pad row)))))))))))

(defun align-buffer--finish-placement (session window side)
  "Scroll WINDOW the rest of the way to level after a placement.

A placement lands on a row boundary, and no boundary falls inside the rows a
pad hangs, so with a pad in the way it stops short - by a whole pad's height
where the counterpart sits just below one.  Scrolling through the pad's own
display covers the rest.  Nothing to do where the placement was already
level, which is every placement with no pad between the two."
  (when (and (window-live-p window)
             (align-buffer--pads-in-p (window-buffer window))
             (align-buffer--exact-p session))
    (let* ((leading (align-buffer-other-side side))
           (leader (get-buffer-window (align-buffer-buffer session leading)))
           (target (and leader (window-live-p leader)
                        (align-buffer--target session leading leader window))))
      (when target
        (pcase-let ((`(,position . ,pixels) target))
          ;; Only where the counterpart's own line hangs the pad.  Finishing a
          ;; placement more widely than that overrides the rules it placed by,
          ;; and measurement has it holding point against the keys.
          (when (align-buffer-pad-at position (window-buffer window))
            (align-buffer--level-window window position pixels)))))))

(defun align-buffer--level-window (window position pixels)
  "Scroll WINDOW until POSITION is drawn PIXELS below its top.
Return non-nil when it had to move.  Bounded, since a pixel the window cannot
show POSITION at would otherwise be chased for ever."
  (let ((tries 0)
        (out (- (align-buffer--offset-in window position) pixels)))
    (while (and (not (zerop out)) (< tries 4))
      (align-buffer-scroll window out)
      (setq tries (1+ tries))
      (setq out (- (align-buffer--offset-in window position) pixels)))
    (> tries 0)))

(defun align-buffer--pad-after-place (session window side)
  "Finish the placement of WINDOW, which shows SIDE of SESSION.

Both parts in one go, and the window noted as ours again at the end: a
placement is noted before this runs, so a start or a point we change here
leaves that note stale, and a stale note reads as a reader having moved the
window - which hands it the lead and has the two panes placing each other."
  (align-buffer--finish-placement session window side)
  (align-buffer--show-placed-cursor session window side)
  (when (window-live-p window)
    (align-buffer--note-placed window (window-start window)
                               (window-point window))))

(add-hook 'align-buffer-post-place-functions #'align-buffer--pad-after-place)

(defun align-buffer--walk-kept-p ()
  "Return non-nil while the walk of a pad is still where the reader is.

Point stays on the line a pad hangs on for as long as its rows are walked,
which is what lets any command whatever be told apart from a step of the
walk: a point on another line is a command having moved it, and the walk is
over.  Told apart by what the command did rather than by its name, so the
unbounded set of commands that move point needs no list, and a command that
moves neither point nor the view - a prefix key, `execute-extended-command',
a look at the documentation - leaves the reader where they were.

A step taken by the command just run is the one case where point elsewhere on
that line is not the reader's doing: another package has adjusted the cursor
between the step and this, as evil does off the end of a line, and the row is
still what the reader asked for."
  (pcase align-buffer--walked
    (`(,overlay . ,_)
     (and (overlay-buffer overlay)
          (eq (current-buffer) (overlay-buffer overlay))
          (let ((hangs (align-buffer-pad-hangs overlay)))
            (or (= (point) hangs)
                (and align-buffer--walk-stepped
                     (= (line-beginning-position)
                        (save-excursion (goto-char hangs)
                                        (line-beginning-position))))))))))

(defun align-buffer--walk-hold-point ()
  "Put point back where the rows being walked are drawn.

A row carries the cursor through the one glyph of it point stands on, so
point left anywhere else draws the cursor on the text instead and the reader
sees nothing of the row they are reading.  What moves it there is another
package adjusting the cursor once the command is over - evil pulls it back
off the end of a line after every command - so this runs after everything
else in the hook, and takes back only that much: a walk the check before it
has already ended is over, and point on another line is the reader's own."
  (pcase align-buffer--walked
    (`(,overlay . ,_)
     (when (and (overlay-buffer overlay)
                (eq (current-buffer) (overlay-buffer overlay)))
       (let ((hangs (align-buffer-pad-hangs overlay)))
         (when (and (/= (point) hangs)
                    (= (line-beginning-position)
                       (save-excursion (goto-char hangs)
                                       (line-beginning-position))))
           (goto-char hangs)
           (dolist (window (get-buffer-window-list (current-buffer) nil t))
             (set-window-point window hangs))))))))

(defun align-buffer--settle-scroll (window)
  "Put the cursor on the row of a pad WINDOW has just been scrolled to.
Return non-nil when a row of a pad took it, and nil where the view has none to
read.

A scroll says where the view is, and the row being read follows it: it stays
where it is on screen while the view moves under it, and moves only once the
view has gone past it.  The pairs cannot say any of this, the line a pad hangs
on being below everything the window is showing.

Called as the scroll is made, and not merely once the command that made it is
over.  The rows are display and the cursor on one of them is a property of
that display, so a redisplay anywhere inside that command draws the cursor
wherever it was last asked for; a cursor outside the view is one redisplay
scrolls the window to, and the scroll it makes puts the window start inside
the pad's own overlay, where not one of its rows is drawn."
  (with-current-buffer (window-buffer window)
    (let* ((band (align-buffer--band window))
           ;; Where the cursor is to stay, for a scroll that keeps it on its
           ;; screen row, and the top of the band for one that does not: that
           ;; is where the reader is looking either way.
           (kept align-buffer-scroll-kept-pixels)
           (at (or kept (car band)))
           ;; The pad drawn there, which is not always the one the window
           ;; starts inside: a scroll can leave a line and then a pad taller
           ;; than the window, and those rows are then all there is to read.
           (pad (align-buffer--pad-showing window at))
           (was (and (null kept) align-buffer--walked
                     (eq (car align-buffer--walked) pad)
                     (cdr align-buffer--walked)))
           (row (and pad (align-buffer--pad-row-in-band
                          window pad
                          (or was (align-buffer--pad-row-at window pad at))))))
      (when row
        (align-buffer--walk-into pad row window)
        t))))

(defun align-buffer--walking-p ()
  "Return non-nil while the rows of a pad are being read on either side.

The rows may be the facing pane's rather than this one's: a reader standing in
a run of lines the other side has not got has that side walking the pad which
stands for them, and holds both panes while it lasts.  So a scroll with no row
left to read has as much to give back there as here."
  (or align-buffer--walked align-buffer-inhibit-sync))

(defun align-buffer--walk-check ()
  "Keep or end the walk of a pad, by what the command just run did.

A nil command is no command of the reader's: the hook runs again with nothing
in `this-command' between one command and the next, and a walk has to live
through that to last longer than a single press."
  (when this-command
    ;; Before the view is read, since what the scroll asked for is what the
    ;; rows are measured against: the command that scrolled may have taken
    ;; the lift off again on its way out.
    (align-buffer-keep-scrolled-view)
    (let* ((scrolled align-buffer--scrolled-window)
           ;; The window the wheel scrolled, which is not always the selected
           ;; one: a notch over the facing pane moves that pane, and the walk
           ;; to keep or end is the one in its own buffer.
           (window (if (window-live-p scrolled) scrolled (selected-window))))
      (setq align-buffer--scrolled-window nil)
      (with-current-buffer (window-buffer window)
        (align-buffer--walk-check-1 window scrolled)
        ;; Once the command it belongs to has been judged, so the next one is
        ;; judged by where point is and nothing else.
        (setq align-buffer--walk-stepped nil)))))

(defun align-buffer--walk-check-1 (window scrolled)
  "Keep or end the walk of a pad in WINDOW, SCROLLED when a scroll moved it."
  (cond
   (scrolled
    ;; A scroll says where the view is, and the row being read follows it: it
    ;; stays where it is on screen while the view moves under it, and moves
    ;; only once the view has gone past it.  The pairs cannot say any of this,
    ;; the line a pad hangs on being below everything the window is showing.
    ;;
    ;; Only from a scroll: a line move that happens to leave the top of the
    ;; window inside some other pad is not a reader walking that pad.
    (cond
     ((align-buffer--settle-scroll window))
     ((align-buffer--walking-p)
      (align-buffer--walk-away)
      ;; The row that was carrying the cursor is gone, so point has to be
      ;; somewhere the window shows again, or redisplay scrolls to it - and
      ;; where the scroll was keeping the cursor on its screen row, that is
      ;; where it goes on this press too: the row it was reading has gone by,
      ;; and a line is drawn where the row was.
      (align-buffer--keep-point-shown window align-buffer-scroll-kept-pixels)
      ;; The pairs have the windows back, so they can say what is level.
      (align-buffer-sync window))
     ((not (eq window (selected-window)))
      ;; A scroll of the pane the reader is not standing in -
      ;; `scroll-other-window' - is still the view they asked for, so that
      ;; pane leads and the one they are in comes level with it.  Nothing
      ;; else speaks for it: the hook that syncs after a command speaks for
      ;; the selected window alone.
      (align-buffer-sync window))))
   ((null align-buffer--walked) nil)
   ((not (align-buffer--walk-kept-p))
    ;; The walk is over, so the pairs have both panes back and owe the facing
    ;; one a placement.  Asked for here, because nothing else will: the hook
    ;; that places after a command goes by what changed since the last one,
    ;; and a reader stepping off the rows onto the line they hang on changes
    ;; neither the view nor the line it reads.
    (align-buffer--walk-away)
    (align-buffer--keep-point-in-band window)
    (align-buffer-sync window))
   ;; A row off the screen is one redisplay scrolls back to, so a walk lasts
   ;; as long as its row is drawn; the facing pane is brought level with the
   ;; row again while it is.
   ((align-buffer--pad-row-shown-p window (car align-buffer--walked)
                                   (cdr align-buffer--walked))
    (align-buffer--pad-face-row window (car align-buffer--walked)
                                (cdr align-buffer--walked)))
   (t
    (align-buffer--walk-away)
    (align-buffer--keep-point-shown window)
    (align-buffer--keep-point-in-band window)
    (align-buffer-sync window)))
  ;; A walked row carries the cursor itself.  Anything else leaves point where
  ;; the pairs cannot speak for it, and two things are then ours: a line
  ;; standing in a run the other side has not got, which faces a row of a pad
  ;; over there, and showing point at all above a pad, which redisplay cannot.
  (unless align-buffer--walked
    ;; The reader's own pane first: a line left inside its margin is one
    ;; redisplay scrolls a row for, and the facing pane has to be brought
    ;; level with the row the line ends up on rather than the one it is
    ;; passing through, or it is left a row out every press.  A scroll has
    ;; already put point on a line drawn clear of the edges, so there is
    ;; nothing there for the band to do and a scroll of its own would only
    ;; take back part of the reader's.
    ;;
    ;; And read from the view the reader is about to be given rather than the
    ;; one still on the screen: a command that moved point out of the window
    ;; leaves the start behind until redisplay catches up.
    (if (align-buffer--point-row-settled-p window)
        (align-buffer--read-view window scrolled)
      (align-buffer--read-view-when-settled window scrolled))))

(defun align-buffer--finish-level (window)
  "Scroll the facing pane the rest of the way to level with WINDOW.

A placement lands on a row boundary and no boundary falls inside the rows a
pad hangs, so a counterpart sitting behind a pad taller than the window is
left about a pad's height short of level.  Done from here and not only from
the hook a placement runs: a placement that finds the window already where it
means to put it moves nothing, runs no hook, and leaves it short for good."
  (let* ((session (align-buffer-current-session))
         (side align-buffer--side)
         (facing (and session side (align-buffer-other-side side)))
         (other (and facing
                     (get-buffer-window (align-buffer-buffer session facing))))
         (target (and (window-live-p other)
                      (align-buffer--target session side window other))))
    (when target
      (pcase-let ((`(,position . ,pixels) target))
        (when (align-buffer--level-window other position pixels)
          (align-buffer--show-placed-cursor session other facing)
          (align-buffer--note-placed other (window-start other)
                                     (window-point other)))))))

(defun align-buffer--point-row-settled-p (window)
  "Return non-nil when WINDOW draws the row its point is on where it will stay.

Inside the window, which is as much as can be asked: a row a little past the
margin is one redisplay scrolls a row for, and waiting for that costs the
facing pane a visible move on nearly every press, where a row outside the
window altogether is a view about to move a long way and everything read from
it speaks for a view nobody will be looking at.

Measured rather than asked of the display, which answers nothing at all where
there is no screen to draw on, and a run with no screen is still worth
checking."
  (let ((top (align-buffer--point-row-top window)))
    (and (>= top 0) (< top (window-body-height window t)))))

(defvar align-buffer--settle-timer nil
  "The one-shot timer set to read a view of WINDOW's once it has come to rest.")

(defun align-buffer--read-view (window scrolled)
  "Draw WINDOW's own line clear of its edges and face whatever run it is in.
SCROLLED says a scroll put the view where it is, which has already left the
line point is on drawn clear.

Read straight away, so the facing pane is put where it belongs in one move
rather than being seen on its way there.  Where the band has just moved the
view, redisplay may move it again and the row faced here is then beside the
wrong line; reading it a second time afterwards puts that right only to have
redisplay move it once more, and waiting for the view instead costs the pane
a visible step on nearly every press.  Both were measured and both were
worse."
  (unless scrolled (align-buffer--keep-point-in-band window))
  (align-buffer--face-run window))

(defvar align-buffer--face-timer nil
  "The one-shot timer set to face a run again after redisplay moved the view.")

(defvar align-buffer--facing-again nil
  "Non-nil while the pass that faces a run after such a scroll is running.")

(defun align-buffer--face-after-scroll (window)
  "Face WINDOW's run again once the scroll redisplay is making is through.

The one thing the hook that places cannot see: a command that writes a view of
its own - a recentre among them - is not finished with the view by the time
that hook runs, and a run faced then stands beside a line the reader is about
to leave.  Redisplay moving the view is what says so, and this is called from
there, so the facing itself waits for a timer.

Where a pad's rows are being read it is the row that is faced again, the
reader standing on it rather than on a line of their own."
  (when (and (window-live-p window)
             (not align-buffer--face-timer)
             (not align-buffer--facing-again))
    (setq align-buffer--face-timer
          (run-with-timer
           0 nil
           (lambda ()
             (setq align-buffer--face-timer nil)
             (when (window-live-p window)
               (with-demoted-errors "align-buffer facing after a scroll: %S"
                 (with-current-buffer (window-buffer window)
                   (let ((align-buffer--facing-again t)
                         (walked align-buffer--walked))
                     (if walked
                         (align-buffer--pad-face-row window (car walked)
                                                     (cdr walked))
                       (align-buffer--face-run window)))))))))))

(setq align-buffer-held-scroll-function #'align-buffer--face-after-scroll)

(defun align-buffer--read-view-when-settled (window scrolled &optional tries)
  "Read WINDOW's view once the redisplay about to move it has been through.

A command that moved point out of the window leaves the start behind until
redisplay catches up, and everything read from that start speaks for a view
nobody will be looking at: the line to draw clear of the edges is drawn
nowhere yet, and a row put beside it stands beside the wrong line.

A timer of no delay runs after that redisplay, whether the view was brought
back by a start of its own or by a lift being cleared, neither of which the
hook that waits on a scroll would hear about."
  (unless align-buffer--settle-timer
    (setq align-buffer--settle-timer
          (run-with-timer
           0 nil
           (lambda ()
             (setq align-buffer--settle-timer nil)
             (when (window-live-p window)
               (with-demoted-errors "align-buffer reading a settled view: %S"
                 (with-current-buffer (window-buffer window)
                   ;; A timer runs between one command and the next, which
                   ;; can be before the redisplay rather than after it, and
                   ;; asking for one from here moves nothing: the view a
                   ;; command asked for is settled by the command loop's own
                   ;; pass.  So a view still on the move waits once more,
                   ;; and a few waits are all it gets.
                   (if (or (>= (or tries 0) 3)
                           (align-buffer--point-row-settled-p window))
                       (align-buffer--read-view window scrolled)
                     (align-buffer--read-view-when-settled
                      window scrolled (1+ (or tries 0))))))))))))

(defun align-buffer--face-run (window)
  "Keep the facing pane level while point is in a run the other side has not got.
The pairs say nothing inside such a run, so the sync would hold the facing
pane at the pair below it; what belongs beside those lines is the rows of the
pad that stands for them, and they are ours to keep level."
  (let* ((session (align-buffer-current-session))
         (side align-buffer--side)
         ;; The window's own point rather than the buffer's: a scroll of the
         ;; pane the reader is not standing in is levelled from here too, and
         ;; a buffer shown in a window it is not selected in keeps a point of
         ;; its own that has nothing to do with what that window shows.
         (line (and session side
                    (align-buffer-line-at (current-buffer)
                                          (window-point window))))
         (facing (and line (align-buffer--facing-pad session side line
                                                    window))))
    (cond
     (facing
      ;; Both panes are ours while the run lasts, for the same reason they are
      ;; while a pad is walked.
      (setq align-buffer-inhibit-sync t)
      (align-buffer--face-facing-row window (car facing) (cdr facing)))
     (align-buffer-inhibit-sync
      (align-buffer--walk-away)
      (align-buffer-sync window)
      (when (and session (align-buffer--exact-p session))
        (align-buffer--finish-level window)))
     ((and session (align-buffer--exact-p session))
      (align-buffer--finish-level window)))))



;;; Answering a window that changes size

;; A window of another width wraps its lines into another number of rows, so
;; every line below the first wrapped one is drawn somewhere else and the two
;; panes no longer read level.  A pad is the sharper case of the same thing:
;; its rows are the display of the facing side's lines, measured in the window
;; drawing them, so until that measurement is taken again the pad stands as
;; tall as the run used to be drawn.  A size change therefore measures the
;; pads and places the panes level afresh, and it places them whether or not a
;; pad came out at another height.
;;
;; Both halves live here because this is the layer that has them: the pads to
;; measure, and the two answers a placement can take.

(defvar align-buffer--resize-timer nil
  "Timer set to answer a size change once redisplay is over.")

(defun align-buffer-pad-lines (overlay)
  "Return how many lines of the facing side OVERLAY's pad stands for.
Its rows say how many screen rows those lines are drawn as over there, which
is as many or more."
  (let ((run (overlay-get overlay 'align-buffer-pad-run)))
    (if run
        (length (cdr run))
      (align-buffer-pad-rows overlay))))

(defun align-buffer--pad-measure-again (overlay)
  "Measure OVERLAY's pad against the facing side as it is drawn now.
Return non-nil when it came out other than the pad stands."
  (let* ((facing (overlay-get overlay 'align-buffer-pad-facing))
         (facing-line (overlay-get overlay 'align-buffer-pad-facing-line))
         (was (overlay-get overlay 'align-buffer-pad-run))
         (run (align-buffer--pad-run-rows facing facing-line
                                          (align-buffer-pad-lines overlay))))
    ;; Nil where those lines are drawn nowhere to be measured, and the pad
    ;; keeps the rows it has: a window the reader has put away says nothing
    ;; about how wide the lines wrap in a window that is still up.
    (when (and run (not (equal run was)))
      (let ((rows (length (car run))))
        (overlay-put overlay 'align-buffer-pad-run run)
        (overlay-put overlay 'align-buffer-pad-rows rows)
        (with-current-buffer (overlay-buffer overlay)
          ;; Drawn afresh at the new count, which is also what puts the cursor
          ;; back on the row being walked - on the last row where the run has
          ;; lost the one it was on.
          (let ((row (when (eq (car-safe align-buffer--walked) overlay)
                       (min (cdr align-buffer--walked) (1- rows)))))
            (when row (setcdr align-buffer--walked row))
            (align-buffer--pad-show-cursor overlay row)))
        t))))

(defun align-buffer--measure-pads-in (buffer)
  "Measure BUFFER's pads against the facing side as it is drawn now.
Return non-nil when any of them came out other than it stood."
  (when (align-buffer--pads-in-p buffer)
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (let ((changed nil))
          (dolist (overlay (overlays-in (point-min) (point-max)))
            (when (and (overlay-get overlay 'align-buffer-pad-rows)
                       (align-buffer--pad-measure-again overlay))
              (setq changed t)))
          changed)))))

(defun align-buffer--level-again (session)
  "Place SESSION's panes level again, as its windows are drawn now.
Led by the window the reader stands in, whose view is the one to keep: what a
line drawn at another height moves is the pane facing it."
  (let* ((windows (delq nil (mapcar
                             (lambda (side)
                               (get-buffer-window
                                (align-buffer-buffer session side)))
                             '(left right))))
         (leader (if (memq (selected-window) windows)
                     (selected-window)
                   (car windows))))
    ;; The placements are noted as they were taken, so a pad of another height
    ;; leaves them saying the panes are level when they are not.
    (mapc #'align-buffer-forget-placement windows)
    (when (window-live-p leader)
      (with-current-buffer (window-buffer leader)
        (let* ((side align-buffer--side)
               (line (align-buffer-line-at (current-buffer)
                                           (window-point leader))))
          ;; The same three answers as a scroll of the reader's.
          (cond
           ;; The row of a pad being read is what the facing pane comes level
           ;; with.
           (align-buffer--walked
            (align-buffer--pad-face-row leader (car align-buffer--walked)
                                        (cdr align-buffer--walked)))
           ;; Inside a run the other side has not got, the rows standing for it
           ;; are, the pairs saying nothing there.
           ((and side line
                 (align-buffer--facing-pad session side line leader))
            (align-buffer--face-run leader))
           ;; Elsewhere the pairs speak, which is the ordinary placement.  A
           ;; scroll gets it from the hook that heard the scroll; a size
           ;; change has to ask for it here, and for the last pixels of it
           ;; after it, a placement landing on a row boundary.
           (t
            ;; A hold taken for a run the view has since left would keep the
            ;; placement from being made at all.
            (when align-buffer-inhibit-sync (align-buffer--walk-away))
            (align-buffer-sync leader)
            (when (align-buffer--exact-p session)
              (align-buffer--finish-level leader)))))))))

(defun align-buffer--measure-and-level ()
  "Measure every session's pads again, and place its panes level afresh."
  (when align-buffer--resize-timer
    (cancel-timer align-buffer--resize-timer)
    (setq align-buffer--resize-timer nil))
  (dolist (session align-buffer--sessions)
    (when (align-buffer-session-live-p session)
      (dolist (side '(left right))
        (align-buffer--measure-pads-in (align-buffer-buffer session side)))
      (align-buffer--level-again session))))

(defun align-buffer--size-changed (frame)
  "Have the panes shown in FRAME measured and placed level again.
Once redisplay is over: this runs from inside it, and a window placed from
there is put back by the pass that follows."
  ;; Errors demoted: Emacs REMOVES a hook function that signals, so one error
  ;; would leave every later size change unanswered.
  (with-demoted-errors "align-buffer resize: %S"
    (when (and (frame-live-p frame)
               (seq-some (lambda (window)
                           (align-buffer-session-for (window-buffer window)))
                         (window-list frame 'no-minibuffer))
               (not align-buffer--resize-timer))
      (setq align-buffer--resize-timer
            (run-with-timer 0 nil #'align-buffer--measure-and-level)))))

(add-hook 'window-size-change-functions #'align-buffer--size-changed)


(provide 'align-buffer-pads)
;;; align-buffer-pads.el ends here
