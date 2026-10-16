;;; align-buffer-sync.el --- Keep the two buffers scrolled to each other -*- lexical-binding: t; -*-

;;; Commentary:

;; The window the user is in leads; the other follows.  The correspondence
;; decides where: we take the pair governing the line point is on and put that
;; pair's two lines on the same screen row.
;;
;; Anchoring on POINT rather than on the window's top line is what makes an
;; unpaired run behave.  While point moves down a block with no counterpart the
;; governing pair does not change, so the other window holds still instead of
;; scrolling through content that has nothing to answer to.  And once that
;; pair's own line scrolls off the top its row is taken as zero, so the other
;; window stops rather than drifting on by the height of the block.
;;
;; A correspondence may be non-monotonic, so the line the follower has to show
;; can be anywhere in its buffer.  Crossing into a moved block makes it jump,
;; which is the whole point of allowing it.
;;
;; Everything is measured in PIXELS, not buffer lines and not rows.  A line
;; takes the room the display gives it: wrapped over several rows, carrying rows
;; an overlay hangs above it, or taller than its neighbours because of its face.
;; None of that is visible to a count of lines, and a count of rows only works
;; while every row is worth the same.
;;
;; What we can ask of a window is coarser than what we can measure of it,
;; though, since a window start is a buffer position and so always a row
;; boundary.  The pixel asked for therefore often falls inside a row, and the
;; nearer boundary is taken.  Closing that gap needs `set-window-vscroll', and
;; redisplay drops a vscroll we set on the following cycle, so the last few
;; pixels are not on offer.
;;
;; FIVE RULES.  Every bug found in the synthetic-pane version of this file was
;; a breach of one of them, and they hold unchanged here:
;;
;; 1. Never act on a viewport reading older than the current redisplay.  Read
;;    the leader when the placement happens, not when it was decided.
;;
;; 2. Redisplay moves a window whose point is inside its `scroll-margin'.  Both
;;    windows are subject to this, so we place the follower's point clear of its
;;    margin AND read the leader's rows as they will settle rather than as they
;;    momentarily are.  Clear of the margin has to hold even where the follower
;;    runs out of lines to offer, which costs it the row we wanted: a placement
;;    redisplay undoes reads as the user scrolling, and the follower then leads
;;    and drags the leader back.
;;
;; 3. Setting a window start is what asks for that window to be painted.  Skip
;;    the write as redundant and the paint is skipped with it, so the paint is
;;    asked for explicitly instead.  And asking from inside redisplay asks
;;    during the cycle already running, which honours nothing and schedules
;;    nothing, so a window moved there is marked again from a timer.
;;
;; 4. A window we placed is not a scroll to follow.  Moving the follower scrolls
;;    it, which fires both hooks for it, and leading from that would undo what
;;    the user just did.  So we record the start and the point we imposed, and a
;;    window sitting on exactly those is left alone.  Reading that record does
;;    not clear it, so both hooks can ask in the same cycle and get the same
;;    answer.
;;
;; 5. Point cannot be moved from `window-scroll-functions'; redisplay reverts
;;    it.  A placement of the selected window is therefore queued, and carried
;;    out by a one-shot timer of no delay, so that it does not wait on the user
;;    pressing another key.

;;; Code:

(require 'align-buffer-core)

(defcustom align-buffer-exact-alignment nil
  "Whether a placement finishes with a vscroll, landing the pair on the pixel.
When nil the nearer row boundary is taken, which is up to half a row out and
settles in one go.

When non-nil the window is put on the boundary below the pixel wanted and
lifted onto it with a vscroll, which lands the pair exactly where a line is
drawn taller than a row, or wrapped differently on the two sides.  The lift
waits `align-buffer--lift-delay' for a redisplay to have been through, so the
window settles in two steps rather than one, and a block of overlay rows is
still out by rows the margin will not let the lift cover."
  :type 'boolean
  :group 'align-buffer)

(defvar align-buffer-post-place-functions nil
  "Functions run with the session, the window just placed, and its side.
Run when a placement moved the window.  No command runs in that window's
buffer, so whatever is drawn from its point is brought up to date from here.")

(defvar align-buffer--syncing nil
  "Non-nil while we are positioning the other window.")

(defvar align-buffer--in-redisplay nil
  "Non-nil while we run inside `window-scroll-functions'.")

(defvar align-buffer--pending-timer nil
  "The one-shot timer set to carry out a queued placement.")

(defvar align-buffer--pending-place nil
  "A queued placement, as (SESSION WINDOW SIDE LEADER).")

(defvar align-buffer-inhibit-sync nil
  "When non-nil, no placement is made and the windows are left alone.")

(defvar align-buffer-held-scroll-function nil
  "A function called with a window redisplay has scrolled, or nil.
Called in place of a placement while `align-buffer-inhibit-sync' says a
consumer is holding both panes, and only for a scroll that is not ours: it is
how such a consumer hears that redisplay has moved a view out from under what
it placed.  Called from inside redisplay, so what it does about it waits.")

(defvar align-buffer-point-for-scroll-function nil
  "A function saying where point belongs after a scroll, given a window.
Where it answers a position, point is put there: a consumer drawing the cursor
on a row of its own needs point on the line that row hangs on, whatever the
scroll margin would otherwise ask.  A nil answer leaves point to be put on a
line drawn clear of the window's edges.

A second argument says how far below the window's top the cursor is to be
drawn, or is nil where the scroll is free to carry the cursor with the text.")

(defvar align-buffer-cursor-pixels-function nil
  "A function of one window saying how far below its top the cursor is drawn.
For a consumer whose cursor is somewhere other than point; a nil answer
leaves it to be measured from point itself.")

(defvar align-buffer-uncounted-height-function nil
  "A function saying what height a span begins inside and is not measured as.
Called with the window and the span's lower and upper position.  For a
consumer drawing rows that hang between two lines: a span beginning inside
such a block is measured as none of the rows, which are drawn below where it
begins all the same.  A nil answer adds nothing.")

(defvar align-buffer--scrolled-view nil
  "The view the last scroll of ours settled on, as (WINDOW START . LIFT).")

(defvar align-buffer-scroll-kept-pixels nil
  "Where the scroll last made was asked to keep the cursor drawn, or nil.
A consumer drawing the cursor itself reads it to put the cursor back where it
was, since a row of its own is not a position `align-buffer-scroll-rows' can
put point at.")

(defun align-buffer--exact-p (session)
  "Return non-nil when SESSION's placements have to land on the pixel.
`align-buffer-exact-alignment' asks it of every session, and a session
carrying `:exact-alignment' asks it for itself: a consumer drawing rows of
its own between the lines cannot be level on a row boundary, no boundary
falling inside those rows."
  (or align-buffer-exact-alignment
      (plist-get (align-buffer-session-properties session) :exact-alignment)))

(defun align-buffer--lift-bound (window &optional position)
  "Return the largest lift worth giving WINDOW, in pixels.

A windowful, since lifting further would hide more than the window shows and
the rows a lift hides still have to be laid out; measured cost is flat up to
there, so nothing smaller buys anything.

Or the whole of POSITION's line, where that line is drawn taller than a
windowful.  A line drawn as more rows than the window has is crossed by
lifting through it, and its rows have to be laid out to draw it at all; a
bound stopping at a windowful would have the pane take the row boundary below
instead and read a whole block out of level."
  (max (* (max 1 (window-body-height window))
          (align-buffer--row-height window))
       (if position (align-buffer-line-display-height window position) 0)))

(defconst align-buffer--lift-delay 0.05
  "How long a lift waits, so that a redisplay has been through before it.
A start written in one cycle clears the vscroll at the redisplay after it, and
a timer of no delay can run before that redisplay rather than after.")


(defvar-local align-buffer--last-start nil
  "Window start this buffer's window had when we last looked.")

(defvar-local align-buffer--last-line-beginning nil
  "Line beginning point was at in this buffer when we last looked.")

(defvar-local align-buffer--last-lift nil
  "Lift this buffer's window had when we last looked.")

(defvar-local align-buffer--last-point nil
  "Where point was in this buffer when we last looked.")

(defvar-local align-buffer--last-tick nil
  "This buffer's text counter when we last looked.")


(defvar align-buffer--last-window nil
  "The window the last command ran in, so that entering the other pane is seen.")

(defvar-local align-buffer--cursor-line-setting 'unset
  "What this buffer held in `make-cursor-line-fully-visible', or `unset'.")


;;; Measuring a window

(defun align-buffer--row-height (window)
  "Return what one screen row of WINDOW is worth in pixels."
  (with-current-buffer (window-buffer window)
    (max 1 (default-line-height))))

(defun align-buffer--window-rows (window)
  "Return how many rows of WINDOW's own buffer its body holds.
Counted with the height those rows are drawn at rather than with
`window-body-height', which divides by the frame's default: a buffer whose
rows are taller than that - `line-spacing', a taller default face - draws
fewer rows than the window reports."
  (max 1 (/ (window-body-height window t) (align-buffer--row-height window))))

(defun align-buffer--pixels-between (window from to)
  "Return the height in pixels of WINDOW's text from FROM to TO."
  ;; `window-text-pixel-size' takes the window's vscroll off whatever it
  ;; reports, wherever the span begins, so it answers where text is drawn
  ;; rather than what it is worth.  Adding it back makes a span a span: a
  ;; measurement that moved with the lift already applied would have every lift
  ;; chasing the last one.
  (+ (cdr (window-text-pixel-size window (min from to) (max from to)))
     (window-vscroll window t)
     (align-buffer--uncounted-height window (min from to) (max from to))))

(defun align-buffer--uncounted-height (window from to)
  "Return what WINDOW draws between FROM and TO that the span misses."
  (or (and align-buffer-uncounted-height-function
           (funcall align-buffer-uncounted-height-function window from to))
      0))


(defun align-buffer--hanging-height (window position)
  "Return the height of what hangs at POSITION above the text there.
Measured as a span of no width, which is where rows hanging at a position are
counted and nothing else is."
  (align-buffer--pixels-between window position position))

(defun align-buffer-line-display-height (window position)
  "Return the pixel height of the display of the line POSITION is on in WINDOW.
Everything that line is drawn as, the rows an overlay hangs on it included.

Measured to the beginning of the next line, less what hangs there.  Rows
hanging at a position are counted at either edge of a span, so the span to
the next line carries that line's rows and they have to come off again; and
measuring to this line's own end instead is no answer, since on an empty line
that span has no width and comes to nothing at all."
  (with-current-buffer (window-buffer window)
    (save-restriction
      (widen)
      (save-excursion
        (goto-char position)
        (let* ((from (line-beginning-position))
               (next (and (zerop (forward-line 1)) (point))))
          (if next
              (- (align-buffer--pixels-between window from next)
                 (align-buffer--hanging-height window next))
            ;; Nothing below the last line to hang anything of its own, so
            ;; the span to the end of the buffer is the line and no more.
            (align-buffer--pixels-between window from (point-max))))))))

(defun align-buffer-scroll (window pixels)
  "Scroll WINDOW down by PIXELS, negative meaning up.
Return the start and the lift it settled on, as (START . LIFT).

A line is crossed by lifting through its own display rather than by stepping
over it, so a line drawn as eighty rows takes eighty rows of scrolling to
pass, which is what the ordinary scroll commands cannot do.

Point is left where it is: a caller scrolling on its own account may have
something else keeping the cursor on screen, and redisplay throws the start
away when nothing does.  `align-buffer-scroll-rows' brings point along."
  (let* ((buffer (window-buffer window))
         (start (with-current-buffer buffer
                  (save-excursion (goto-char (window-start window))
                                  (line-beginning-position))))
         (want (+ (window-vscroll window t) pixels)))
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (if (>= want 0)
            (let ((height (align-buffer-line-display-height window start))
                  (next (align-buffer--line-after start)))
              (while (and next (>= want height))
                (setq want (- want height))
                (setq start next)
                (setq height (align-buffer-line-display-height window start))
                (setq next (align-buffer--line-after start)))
              ;; Nothing below to scroll into, so the lift goes no further
              ;; than leaves the last of what that line is drawn as on the
              ;; top row: lifting past that leaves an empty window, which is
              ;; further than a reader can scroll.  Everything the line is
              ;; drawn as counts, the rows of a pad hanging off the end of it
              ;; included, so a run the other side alone has at the very end
              ;; is scrolled through like any other.
              (unless next
                (setq want
                      (min want
                           (max 0 (- (align-buffer-line-display-height
                                      window start)
                                     (align-buffer--row-height window)))))))
          (let ((previous (align-buffer--line-before start)))
            (while (and previous (< want 0))
              (setq start previous)
              (setq want (+ want (align-buffer-line-display-height window
                                                                   start)))
              (setq previous (align-buffer--line-before start)))
            (setq want (max 0 want))))))
    (set-window-start window start)
    ;; Preserved, so that a start written in this very cycle does not take the
    ;; lift with it at the next redisplay.
    (set-window-vscroll window want t t)
    (cons start want)))

(defun align-buffer--line-after (position)
  "Return where the line after POSITION's begins, or nil at the end."
  (save-excursion
    (goto-char position)
    (and (zerop (forward-line 1)) (not (eobp)) (point))))

(defun align-buffer--line-before (position)
  "Return where the line before POSITION's begins, or nil at the beginning."
  (save-excursion
    (goto-char position)
    (and (zerop (forward-line -1)) (point))))

(defun align-buffer--band (window)
  "Return the pixels of WINDOW a line can be drawn on, as (TOP . BOTTOM).

The scroll margin is a number of rows, and redisplay keeps clear that many
rows measured from the window's own edges.  A window whose height is not a
whole number of rows therefore protects a taller band than its pixel height
less the margin, and a line left in that band is one redisplay scrolls the
window to get out of."
  (let* ((height (align-buffer--row-height window))
         (margin (align-buffer-effective-margin window))
         ;; A band built on the number the window reports reaches past the
         ;; rows the margin protects, leaving a line deep inside it called
         ;; clear.
         (rows (align-buffer--window-rows window)))
    (cons (* margin height)
          (* (max 1 (- rows margin)) height))))

(defun align-buffer--line-below (window pixels)
  "Return the first line of WINDOW drawn at or below PIXELS from its top.
Nil where the buffer runs out first, which is a window near the end holding
fewer lines than the margin: there is then nowhere for point to go that
redisplay would like better than where it already is.

Walked down from the window's start, so a line drawn as many rows is stepped
over once rather than row by row."
  (let ((position (window-start window)))
    (with-current-buffer (window-buffer window)
      (save-restriction
        (widen)
        (while (and position
                    (let ((drawn (align-buffer--drawn-at window position)))
                      (and drawn (< drawn pixels))))
          (setq position (align-buffer--line-after position)))))
    position))

(defun align-buffer--line-above (window pixels)
  "Return the last line of WINDOW drawn whole above PIXELS from its top.
Walked back from point, which is the line wanted or a few below it, and no
further back than the window's own start."
  (let* ((height (align-buffer--row-height window))
         (start (window-start window))
         (position (with-current-buffer (window-buffer window)
                     (save-excursion (goto-char (window-point window))
                                     (line-beginning-position))))
         (previous t))
    (with-current-buffer (window-buffer window)
      (save-restriction
        (widen)
        (while (and previous
                    (let ((drawn (align-buffer--drawn-at window position)))
                      (or (null drawn) (> (+ drawn height) pixels))))
          (setq previous (align-buffer--line-before position))
          (if (and previous (>= previous start))
              (setq position previous)
            (setq previous nil)))))
    position))

(defun align-buffer--point-in-band (window band)
  "Return where point belongs in WINDOW, moved as little as possible into BAND.

Point is left alone where it already sits on a line BAND leaves room for.
Out of the band by a margin it comes back by a margin, and from outside the
window altogether it comes to the top, which is where a scroll that long has
brought the text the reader was reading."
  (let* ((height (align-buffer--row-height window))
         (at (window-point window))
         (drawn (align-buffer--drawn-at window at)))
    (cond
     ((or (null drawn) (< drawn (car band))
          (> drawn (window-body-height window t)))
      (or (align-buffer--line-below window (car band)) at))
     ((> (+ drawn height) (cdr band))
      (align-buffer--line-above window (cdr band)))
     (t at))))

(defun align-buffer--row-start (window position)
  "Return where the screen row WINDOW draws POSITION on begins."
  (with-current-buffer (window-buffer window)
    (save-excursion
      (goto-char position)
      (vertical-motion 0 window)
      (point))))

(defun align-buffer--cursor-pixels (window)
  "Return how far below WINDOW's top the cursor is drawn, or nil when nowhere.

Measured from the beginning of the row point is drawn on, since a span
measured to a place part way along a row counts that row whole: point in the
middle of a row would read as a row below where the cursor is drawn, and a
scroll keeping the cursor there would walk it down the window."
  (or (and align-buffer-cursor-pixels-function
           (funcall align-buffer-cursor-pixels-function window))
      (align-buffer--drawn-at window
                              (align-buffer--row-start
                               window (window-point window)))))

(defun align-buffer--cursor-pixels-to-keep (window)
  "Return where WINDOW draws its cursor, when a scroll is to keep it there.
Nil where `scroll-preserve-screen-position' asks for nothing.  Measured
before the scroll, the window's height being what it is after it too."
  (and scroll-preserve-screen-position
       (align-buffer--cursor-pixels window)))

(defun align-buffer--point-in-column (window position)
  "Return where POSITION's line stands in the column WINDOW's point is in.
So that a cursor kept on its screen row is kept in its column as well, which
is what the scroll commands of Emacs do with the row they keep."
  (with-current-buffer (window-buffer window)
    (save-excursion
      (let ((column (progn (goto-char (window-point window))
                           (current-column))))
        (goto-char position)
        (move-to-column column)
        (point)))))

(defun align-buffer--line-covering (window pixels)
  "Return the line of WINDOW whose display covers PIXELS below its top.
The line before the first one drawn at or below PIXELS, where that one is
drawn lower down: a line that wraps is drawn across several rows and covers
PIXELS from above."
  (let* ((below (align-buffer--line-below window pixels))
         (drawn (and below (align-buffer--drawn-at window below))))
    (if (and drawn (> drawn pixels))
        (or (align-buffer--line-before below) below)
      below)))

(defun align-buffer--point-at-row (window position pixels)
  "Return where POSITION's line is drawn PIXELS below WINDOW's top.
POSITION is where that line begins, and the row wanted is reached by stepping
through the line's own text and no further: what hangs on it is drawn there
too, and no position of the line falls inside that."
  (let* ((height (align-buffer--row-height window))
         (drawn (align-buffer--drawn-at window position))
         (rows (and drawn (/ (- pixels drawn) height))))
    (if (or (null rows) (<= rows 0))
        position
      (with-current-buffer (window-buffer window)
        (save-excursion
          (goto-char position)
          (let ((end (line-end-position)))
            (vertical-motion rows window)
            (min (point) end)))))))

(defun align-buffer--point-for-pixels (window pixels)
  "Return where point goes for the cursor to be drawn PIXELS below WINDOW's top.
Nil where the window has no line there, which is a window showing the end of
the buffer above its last row.

A line drawn across the place wanted gives the row there rather than its own
first row, which is what keeps the cursor still: taking the line after instead
walks it a row down the window at every scroll."
  (let* ((height (align-buffer--row-height window))
         (band (align-buffer--band window))
         (wanted (max (car band) (min pixels (- (cdr band) height))))
         (position (align-buffer--line-covering window wanted))
         (drawn (and position (align-buffer--drawn-at window position))))
    (cond ((null position) nil)
          ((and drawn (>= (- wanted drawn) height))
           (align-buffer--point-at-row window position wanted))
          (t (align-buffer--point-in-column window position)))))

(defun align-buffer--keep-point-shown (window &optional keep)
  "Move WINDOW's point onto a line drawn clear of its edges after a scroll.

KEEP is where the cursor was drawn before the scroll, for a scroll that is to
leave it there; point then goes to the line drawn at that place rather than to
the nearest one the window likes.  A `scroll-preserve-screen-position' of t
asks for that only of a scroll which would move the cursor at all, and any
other value asks for it of every scroll.

Measured rather than asked of the screen, since the glyphs are not laid out
again until the next redisplay.

Inside the scroll margin counts as outside: point left in the margin is point
redisplay scrolls the window for, and what it does then is start the window on
point's own line, which undoes the scroll and leaves the panes apart."
  (let* ((in-band (align-buffer--point-in-band window
                                               (align-buffer--band window)))
         (pixels (and keep
                      (or (/= in-band (window-point window))
                          (not (eq scroll-preserve-screen-position t)))
                      keep))
         (wanted (or (and align-buffer-point-for-scroll-function
                          (funcall align-buffer-point-for-scroll-function
                                   window pixels))
                     (and pixels (align-buffer--point-for-pixels window
                                                                 pixels))
                     in-band)))
    (setq align-buffer-scroll-kept-pixels pixels)
    (unless (= wanted (window-point window))
      (align-buffer--set-point window wanted))))

(defun align-buffer-scroll-rows (rows &optional window)
  "Scroll WINDOW by ROWS screen rows, down for positive ROWS.
Through whatever the lines are drawn as, so rows an overlay adds are scrolled
through rather than jumped over."
  (interactive "p")
  (let* ((window (or window (selected-window)))
         (keep (align-buffer--cursor-pixels-to-keep window))
         (where (align-buffer-scroll window
                                     (* rows (align-buffer--row-height
                                              window)))))
    (align-buffer--keep-point-shown window keep)
    (setq align-buffer--scrolled-view (cons window where))
    where))

(defun align-buffer--forget-scrolled-view ()
  "Forget the view of a scroll that no command is carrying out.

A scroll owns the view until the command that made it is over, and no longer:
a scroll made from a timer, from a hook or from a reader's own expression
belongs to nothing that is about to put point anywhere, and writing its view
again at the end of some later command would take that command's own view
away - a `recenter' among them, which is then a key that does nothing."
  (setq align-buffer--scrolled-view nil))

(defun align-buffer-keep-scrolled-view ()
  "Put the view the last scroll of ours settled on back where we left it.

A scroll is the view the reader asked for, and a scroll command goes on to put
point where it wants that view read from.  What it does then costs it part of
the scroll: the C code clears the lift wherever a window start is written, so
`recenter' and `set-window-start' and everything built on them lose whatever
the scroll had got inside a line, and a start written to bring point into
view loses the rest.  So the view is written again once the command that asked
for it is over, and until the next scroll: a scroll of ours owns the view, and
one command cannot ask for two."
  (pcase align-buffer--scrolled-view
    (`(,window ,start . ,lift)
     (setq align-buffer--scrolled-view nil)
     (when (and (window-live-p window)
                (or (/= (window-start window) start)
                    (/= (window-vscroll window t) lift)))
       (set-window-start window start)
       (set-window-vscroll window lift t t)))))

(defun align-buffer-scroll-up-row (&optional rows)
  "Scroll ROWS screen rows nearer the end of the buffer, one by default."
  (interactive "p")
  (align-buffer-scroll-rows (or rows 1)))

(defun align-buffer-scroll-down-row (&optional rows)
  "Scroll ROWS screen rows nearer the beginning of the buffer, one by default."
  (interactive "p")
  (align-buffer-scroll-rows (- (or rows 1))))

(defun align-buffer-scroll-up-page ()
  "Scroll a windowful nearer the end of the buffer, less the context lines."
  (interactive)
  (align-buffer-scroll-rows (max 1 (- (window-body-height) next-screen-context-lines))))

(defun align-buffer-scroll-down-page ()
  "Scroll a windowful nearer the beginning of the buffer."
  (interactive)
  (align-buffer-scroll-rows
   (- (max 1 (- (window-body-height) next-screen-context-lines)))))

(defun align-buffer--rows-apart (window from to)
  "Return how many screen rows WINDOW puts between FROM and TO, FROM above TO.
Walked, and no further than the window is tall."
  (with-current-buffer (window-buffer window)
    (save-excursion
      (goto-char from)
      (let ((rows 0)
            (height (max 1 (window-body-height window))))
        (while (and (< (point) to)
                    (< rows height)
                    (not (zerop (vertical-motion 1 window))))
          (setq rows (1+ rows)))
        rows))))

(defun align-buffer--rows-from-start (window position)
  "Return the screen row POSITION sits on in WINDOW, or nil when it is above.
Nil as well when it is further below than the window is tall, so the measuring
covers at most a windowful."
  (let* ((start (window-start window))
         (height (max 1 (window-body-height window)))
         (row-height (align-buffer--row-height window)))
    (when (>= position start)
      ;; Everything above POSITION's own row, which is what the rows before it
      ;; in the window come to, including any an overlay hangs there.
      (let* ((limit (* (1+ height) row-height))
             (pixels (+ (cdr (window-text-pixel-size window start position
                                                     nil limit))
                        (align-buffer--uncounted-height window start position)))
             (row (round pixels row-height)))
        (when (< row height) row)))))

(defun align-buffer--position-rows-below (window from rows)
  "Return the position ROWS screen rows below FROM in WINDOW, ROWS up when it
is negative.  Also how many rows that turned out to be, as (POSITION . ROWS),
which is fewer against either end of the buffer."
  ;; Walked rather than measured, because this is the margin's arithmetic and
  ;; not the alignment's: `vertical-motion' counts a wrapped line's rows and a
  ;; line drawn taller than its neighbours as the one row it is, and it is only
  ;; the rows an overlay adds that it walks past without counting.  Being a row
  ;; out about those costs a point placed a row nearer an edge than we meant,
  ;; where measuring instead would cost a pixel reading of the whole span on
  ;; every step of every placement.
  (with-current-buffer (window-buffer window)
    (save-excursion
      (goto-char from)
      (let ((moved (vertical-motion rows window)))
        (cons (point) moved)))))

(defun align-buffer--start-already-there (window position pixels)
  "Return (START . VSCROLL) using the start WINDOW already has, or nil.

Keeping the start and changing only the lift is what makes a placement land in
one go: a start written in one cycle clears the lift at the next, so the two
cannot be set together.  Nil when the start it has cannot be lifted onto the
pixel, or would have to be lifted further than a windowful, which is as far as
we are willing to lay out rows nobody is going to see."
  (let ((start (window-start window))
        (height (max 1 (window-body-height window))))
    (when (and align-buffer-exact-alignment (< start position))
      ;; Walked first, and no further than a windowful: a start further off
      ;; than that is both too far to lift from and too far to measure, since
      ;; the pixels have to be laid out to be counted.
      (when (< (align-buffer--rows-apart window start position) height)
        (let ((span (align-buffer--pixels-between window start position))
              (bound (align-buffer--lift-bound window start)))
          (when (and (>= span pixels) (<= (- span pixels) bound))
            (cons start (- span pixels))))))))

(defun align-buffer--already-level-p (window position pixels point)
  "Return non-nil when WINDOW already draws POSITION PIXELS below its top.
POINT has to be where the placement would put it as well: a pane that is level
but reads from the wrong place still has to be put right.

More than one view can draw the pair on the same pixel, and a start walked to
from one of them names another: writing it every time leaves the pane placed
to one and then the other by turns, which a reader sees as a pane that will
not settle."
  (let ((start (window-start window)))
    (and (= (window-point window) point)
         (>= position start)
         (= (+ (align-buffer--pixels-between window start position)
               (align-buffer--uncounted-height window start position))
            pixels))))

(defun align-buffer--start-for-pixels (window position pixels)
  "Return a start for WINDOW putting POSITION PIXELS below its top, and what
is left over for a vscroll to take off, as (START . VSCROLL).

A start is a position and a position is a row boundary, so the pixel asked for
usually falls inside a row.  A vscroll only ever lifts the text, so the start
chosen is the boundary at or below the pixel and the remainder is positive;
with `align-buffer-exact-alignment' nil the nearer boundary is taken instead
and nothing is left over."
  (if (<= pixels 0)
      (cons position 0)
    (with-current-buffer (window-buffer window)
      (save-excursion
        ;; A guess at the usual row height, which is already the answer
        ;; wherever every row between the two is worth it, and a row or two out
        ;; where one is not.
        (goto-char (car (align-buffer--position-rows-below
                         window position
                         (- (round pixels (align-buffer--row-height window))))))
        (if (not align-buffer-exact-alignment)
            (cons (align-buffer--start-nearest window position pixels) 0)
          (let* ((exact (align-buffer--start-at-or-below window position pixels))
                 (bound (align-buffer--lift-bound window (car exact))))
            (if (<= (cdr exact) bound)
                exact
              ;; More lift than it is worth: the nearer boundary instead, which
              ;; lands in one go and in one step.
              (goto-char (car exact))
              (cons (align-buffer--start-nearest window position pixels) 0))))))))

(defun align-buffer--start-at-or-below (window position pixels)
  "Return the start nearest PIXELS without being above it, and the rest.
Point begins at a guess and the walk is from there, as (START . VSCROLL)."
  (let ((best (point))
        (best-pixels (align-buffer--pixels-between window (point) position))
        (done nil))
    (if (>= best-pixels pixels)
        ;; Far enough already: down while the next one is still far enough.
        (while (not done)
          (let ((here (point)))
            ;; Never past POSITION itself.  A span is measured between two
            ;; places whichever way round they are, so a candidate below it
            ;; measures the way back up and reads as far enough again, and the
            ;; walk would carry on to the end of the buffer.
            (if (or (zerop (vertical-motion 1 window))
                    (>= (point) position))
                (progn (goto-char here) (setq done t))
              (let ((got (align-buffer--pixels-between window (point) position)))
                (if (>= got pixels)
                    (setq best (point) best-pixels got)
                  (goto-char here)
                  (setq done t))))))
      ;; Too near: up until it is far enough, and stop there.
      (while (not done)
        (if (zerop (vertical-motion -1 window))
            (setq done t)
          (setq best (point))
          (setq best-pixels (align-buffer--pixels-between
                             window (point) position))
          (when (>= best-pixels pixels) (setq done t)))))
    (cons best (max 0 (- best-pixels pixels)))))

(defun align-buffer--start-nearest (window position pixels)
  "Return the start putting POSITION nearest PIXELS below the top of WINDOW.
Point begins at a guess and the walk is from there."
  (let ((best (point))
        (best-pixels (align-buffer--pixels-between window (point) position))
        (done nil))
    (unless (= best-pixels pixels)
      (let ((step (if (< best-pixels pixels) -1 1))
            (best-out (abs (- best-pixels pixels))))
        (while (not done)
          ;; Never past POSITION itself: a span is measured between two places
          ;; whichever way round they are, so a candidate below it reads as a
          ;; span again rather than as nothing.
          (if (or (zerop (vertical-motion step window))
                  (and (> step 0) (>= (point) position)))
              (setq done t)
            (let* ((got (align-buffer--pixels-between window (point) position))
                   (out (abs (- got pixels))))
              (when (< out best-out)
                (setq best (point))
                (setq best-out out))
              ;; Past the pixel asked for, and further on only gets worse.
              (when (if (< step 0) (>= got pixels) (<= got pixels))
                (setq done t)))))))
    ;; And the two starts either side of the one the walk settled on, which
    ;; makes it the nearest for certain: the walk steps by screen rows while
    ;; the starts it can take are lines, so a line drawn as several rows can
    ;; have it stop a line short of the best.
    (let ((settled best)
          (best-out (abs (- (align-buffer--pixels-between window best position)
                            pixels))))
      (dolist (step '(-1 1))
        (let ((other (save-excursion
                       (goto-char settled)
                       (and (zerop (forward-line step)) (point)))))
          ;; Never past POSITION: a span is measured between two places
          ;; whichever way round they are, so a start below the line reads as
          ;; a span again rather than as nothing.  POSITION itself is allowed,
          ;; being the nearest start there is where the line above it is drawn
          ;; taller than the pixel asked for.
          (when (and other (<= other position))
            (let ((out (abs (- (align-buffer--pixels-between window other
                                                             position)
                               pixels))))
              (when (< out best-out)
                (setq best other)
                (setq best-out out)))))))
    best))

(defun align-buffer--leader-pixels (window position)
  "Return how far below WINDOW's top POSITION is drawn, in pixels.
In pixels, since a row is only worth its own height and two windows share
neither their rows nor the heights of them.  Zero when POSITION is above the
start, which is what holds the other window still rather than dragging it on.

Where the window is drawn now, with nothing added for where the margin might
take it next: a command moves point to the row its margin allows and leaves the
start alone, and redisplay scrolls a window it has already drawn for no margin
of either edge, so there is nothing further coming to allow for."
  (let ((start (window-start window))
        (height (max 1 (window-body-height window))))
    (if (< position start)
        0
      ;; Bounded, so that a position far below the window costs a windowful of
      ;; measurement rather than the whole buffer, and with whatever the span
      ;; begins inside added back: a window starting inside a block of rows
      ;; draws those rows below its top without the span counting any of them.
      (max 0 (+ (cdr (window-text-pixel-size
                      window start position nil
                      (* (1+ height) (align-buffer--row-height window))))
                (align-buffer--uncounted-height window start position))))))


;;; What the follower has to show

(defun align-buffer--target (session side leader follower)
  "Return what FOLLOWER must show to sit level with LEADER, which shows SIDE.
As (POSITION . PIXELS): the counterpart line's position and how far below the
window's top it belongs.  Nil when the correspondence says nothing about the
line point is on in LEADER."
  (let* ((leader-buffer (window-buffer leader))
         (line (align-buffer-line-at leader-buffer (window-point leader)))
         (anchor (and line (align-buffer-anchor session side line))))
    (when anchor
      (let* ((row-height (align-buffer--row-height follower))
             ;; In pixels, since that is what is being asked for: a count of
             ;; rows times the height of a default one falls short of the
             ;; window wherever its lines are drawn taller than that, and the
             ;; pair would be held that much above where the leader draws it.
             (room (max 0 (- (window-body-height follower t) row-height)))
             (other-position (align-buffer-line-position
                              (window-buffer follower) (cdr anchor)))
             ;; Where the pair is drawn in the leader, whether or not point
             ;; has reached it: a pair above point is still the one the two
             ;; panes have in common, and asking for it where it is drawn
             ;; gets the follower as near level as its own text allows.
             (pixels (align-buffer--leader-pixels
                      leader
                      (align-buffer-line-position leader-buffer
                                                  (car anchor)))))
        (when other-position
          (cons other-position (max 0 (min room pixels))))))))


;;; Whose scroll is it

(defun align-buffer--note-placed (window start point)
  "Remember the START and POINT we imposed on WINDOW."
  ;; Rule 4.  Both values, because we set both and either one changing is the
  ;; user moving.  A start-only record reads as ours after a point-motion
  ;; scroll, whose window start is still stale when the command hook runs, and
  ;; a record read as ours swallows the keypress.
  (set-window-parameter window 'align-buffer-placed (cons start point)))

(defun align-buffer--own-scroll-p (window)
  "Return non-nil when WINDOW sits where we put it rather than where a user did."
  ;; Answered by value, so both hooks can ask in the same cycle and get the
  ;; same answer.  A flag saying "we moved something recently" is cleared by
  ;; whichever command comes next, and when that is the user's own wheel notch
  ;; it costs them the notch.
  (let ((noted (window-parameter window 'align-buffer-placed)))
    (cond ((null noted) nil)
          ((and (= (car noted) (window-start window))
                (or (null (cdr noted))
                    (= (cdr noted) (window-point window))))
           t)
          (t (set-window-parameter window 'align-buffer-placed nil)
             nil))))

(defun align-buffer-forget-placement (window)
  "Forget that we ever placed WINDOW."
  (when (window-live-p window)
    (set-window-parameter window 'align-buffer-placed nil)))


;;; Placing a window

(defun align-buffer--set-point (window position)
  "Put WINDOW's point at POSITION so that it sticks."
  (if (eq window (selected-window))
      (with-current-buffer (window-buffer window) (goto-char position))
    (set-window-point window position)))

(defun align-buffer--clamp-point-row (row start-at-beginning height margin)
  "Return ROW moved as little as possible into the band redisplay leaves alone.
MARGIN rows stay clear at each edge of a HEIGHT-row window."
  ;; No floor at all when the window starts at the first line: the margin is
  ;; there to stop redisplay scrolling the window back, and there is nothing
  ;; above the first line to scroll into.  A floor there drags the follower's
  ;; point MARGIN rows down from a viewport that starts at the top.
  (let* ((lowest (if start-at-beginning 0 margin))
         (highest (max 0 (- height margin 1)))
         (wanted (max lowest (min highest row))))
    ;; A window shorter than twice its margin leaves no band at all, and a row
    ;; outside the window would be worse than one inside a margin.
    (max 0 (min (1- height) wanted))))

(defun align-buffer--follower-point (window counterpart row point-row)
  "Return the point to give WINDOW, whose pair is drawn on ROW.
COUNTERPART itself when the margin leaves it on that row, since that is the
line being read; otherwise as many rows from COUNTERPART as the margin moved
it, which is as near to the pair as the margin allows.

Counted from the pair rather than from the window's start, which is the only
way of counting that is not thrown out by a lift: the rows a lift hides are
often rows an overlay added, and a walk cannot see those at all."
  (if (= row point-row)
      counterpart
    (car (align-buffer--position-rows-below window counterpart
                                            (- point-row row)))))

(defun align-buffer--pixels-above (window position)
  "Return how far down WINDOW can draw POSITION at all, in pixels.

What its own buffer draws above it, where that is less than the window: no
start reaches back past the beginning of a buffer, so a position with little
above it cannot be put further down than that little.  The window's height
where there is more above than that, nothing then holding it back.

Measured from a windowful back rather than from the beginning, a span over a
whole buffer being too dear to take for every placement."
  (let* ((body (window-body-height window t))
         (buffer (window-buffer window))
         (back (with-current-buffer buffer
                 (save-restriction
                   (widen)
                   (car (align-buffer--position-rows-below
                         window position
                         (- (1+ (align-buffer--window-rows window)))))))))
    (if (and back (> back (with-current-buffer buffer
                            (save-restriction (widen) (point-min)))))
        body
      (with-current-buffer buffer
        (save-restriction
          (widen)
          (min body (+ (align-buffer--pixels-between window (point-min)
                                                     position)
                       (align-buffer--uncounted-height window (point-min)
                                                       position))))))))

(defun align-buffer--start-leaving-room (window start rows)
  "Return START, moved back far enough that ROWS screen rows fit below it.
The end of the buffer leaves fewer, and then the row we wanted for the anchor
is not on offer: a point that near the bottom sits inside the margin, so
redisplay scrolls the window and takes our placement with it."
  (pcase-let ((`(_ . ,available)
               (align-buffer--position-rows-below window start rows)))
    (if (>= available rows)
        start
      (car (align-buffer--position-rows-below
            window start (- available rows))))))

(defun align-buffer-forget-vscroll (window)
  "Put WINDOW back to no vscroll."
  (when (window-live-p window)
    (set-window-vscroll window 0 t)))

(defun align-buffer--place (session window side leader)
  "Place WINDOW, which shows SIDE of SESSION, level with LEADER.
Nothing while `align-buffer-inhibit-sync' is on, queued placements included:
a consumer holding the windows itself would otherwise be pulled back and
forth by a placement queued before it took over."
  ;; Rule 5.  No placement is made from inside redisplay at all.  The
  ;; selected window's point would be reverted, and a start whose point is
  ;; still inside the margin is one redisplay scrolls away by itself, so the
  ;; two have to land together or not at all; and a lift written inside a
  ;; redisplay pass has its preserve flag taken by that very pass, so the
  ;; pass after it wipes the lift and leaves the pane a lift away from level
  ;; with nothing to say so.
  ;;
  ;; Nil from the deferred branch, since nothing has moved yet: the timer that
  ;; carries the placement out runs with the command loop free, where the
  ;; writes last and where an ordinary redisplay paints them.
  (cond
   (align-buffer-inhibit-sync nil)
   (align-buffer--in-redisplay
    (align-buffer--defer-place session window side leader)
    nil)
   (t (align-buffer--place-now session window side leader))))

(defun align-buffer--place-now (session window side leader)
  "Place WINDOW, which shows SIDE of SESSION, level with LEADER.
Return non-nil when it moved, and nil when it was already there.

Nothing while `align-buffer-inhibit-sync' is on.  Every placement comes
through here, queued ones included, so this is where a consumer holding the
windows itself is sure of being left alone."
  (let* (;; Bound for everything this placement calls, which is how a session
         ;; that needs the pixel gets it without every measurement being
         ;; handed the session to ask.
         (align-buffer-exact-alignment (align-buffer--exact-p session))
         (target (and (not align-buffer-inhibit-sync)
                      (align-buffer--target
                       session (align-buffer-other-side side) leader window))))
    (when target
      (pcase-let*
          ((`(,counterpart . ,pixels) target)
           (`(,wanted . ,vscroll)
            (or (align-buffer--start-already-there window counterpart pixels)
                (align-buffer--start-for-pixels window counterpart pixels)))
           (buffer (window-buffer window))
           (height (max 1 (window-body-height window)))
           (margin (align-buffer-effective-margin window))
           ;; The row the target asks the pair to be drawn on.  Divided, as
           ;; the margin's arithmetic is throughout: it decides only whether
           ;; point has to come off the pair, and by how many rows.
           (aimed (round pixels (align-buffer--row-height window)))
           (start (align-buffer--start-leaving-room
                   window wanted
                   (align-buffer--clamp-point-row
                    aimed
                    (with-current-buffer buffer (= wanted (point-min)))
                    height margin)))
           ;; Moving the start back for the point row scrolls the window by
           ;; whole rows, and the remainder we measured is no longer its.
           (vscroll (if (= start wanted) vscroll 0))
           ;; The row the pair ends up on, which is the row the target asks
           ;; for save where no start of this window can reach it: a pair with
           ;; less than that drawn above it in its own buffer comes no further
           ;; down than what is above it, there being no start further back to
           ;; reach for.  Read of the buffer rather than of any start, the
           ;; start being moved about below and the view that meets the target
           ;; restored afterwards by the levelling.
           (row (round (min pixels (align-buffer--pixels-above window
                                                               counterpart))
                       (align-buffer--row-height window)))
           (point-row (align-buffer--clamp-point-row
                       row
                       (with-current-buffer buffer (= start (point-min)))
                       height margin))
           (point (align-buffer--follower-point window counterpart row
                                                point-row)))
        ;; A buffer the reader has narrowed cannot be shown from outside the
        ;; part they left accessible, so we leave such a window alone rather
        ;; than writing a start redisplay would have to undo.
        ;;
        (when (with-current-buffer buffer
                (and (>= start (point-min)) (<= start (point-max))))
          (let ((moved (not (or (align-buffer--already-placed-p
                                 window start point vscroll)
                                (align-buffer--already-level-p
                                 window counterpart pixels point))))
                (lifted nil)
                (start-stays (= (window-start window) start)))
            (when moved
              ;; Point first, so redisplay never sees the new start with a point
              ;; the margin would make it scroll away from.
              (align-buffer--set-point window point)
              (cond
               (start-stays
                ;; Preserved, so the redisplay after this keeps it.  A lift
                ;; left to be wiped is a lift gone by the time redisplay looks
                ;; at point: point is then in the margin, which it scrolls out
                ;; of by recentering, and a window it recentered reads as one
                ;; the reader moved.
                (set-window-vscroll window vscroll t t))
               (t
                (set-window-start window start)
                ;; Preserved for the same reason, and written here rather than
                ;; waited for: a lift a second pass puts back is a lift the
                ;; redisplay in between did without, and that redisplay is the
                ;; one that recenters the window and takes the lead with it.
                (set-window-vscroll window vscroll t t))))
            ;; A start is a row boundary, and no boundary falls inside the
            ;; rows an overlay hangs on a line: asked for a pixel inside
            ;; those rows, the walk above can only get as near as the line
            ;; itself and the counterpart is left short, by the whole of a
            ;; block where one hangs on it.  Scrolling crosses a line by
            ;; lifting through its display rather than by stepping over it,
            ;; which is how the rest of the way is covered.
            (when align-buffer-exact-alignment
              (setq lifted (align-buffer--level-exactly window counterpart
                                                        pixels)))
            ;; Rule 4, and noted even when nothing was written: the window sits
            ;; where we want it either way, so its next scroll hook is our
            ;; doing.  Skipping it freezes the window.  The start noted is the
            ;; one the window has: a scroll of ours just above may have left it
            ;; somewhere the walk could not name.
            (align-buffer--note-placed window (window-start window)
                                       (window-point window))
            ;; A lift of more than a windowful is one the pass after the one
            ;; we are in wipes, and the pane then sits a lift away from level
            ;; with nothing to say so.  Written again with the command loop
            ;; free, where the preserve flag lasts as it is meant to.
            ;; Rule 3, and asked for even when nothing was written: setting a
            ;; start is what marks a window for painting, so skipping the write
            ;; as redundant skips the paint too, and a follower already in the
            ;; right place goes on showing stale pixels.
            (force-window-update window)
            (when (or moved lifted)
              ;; Last, so that the window is where we want it and marked before
              ;; anyone else looks.  Errors demoted, since we may be inside
              ;; `window-scroll-functions', where Emacs drops a hook that
              ;; signals and the two would stay unsynced from then on.
              (with-demoted-errors "align-buffer post-place: %S"
                (run-hook-with-args 'align-buffer-post-place-functions
                                    session window side)))
            (or moved lifted)))))))

(defun align-buffer--drawn-at (window position)
  "Return how far below WINDOW's top POSITION is drawn.
Nil when POSITION is above the top, where there is no such distance: a span
is measured whichever way round it is given and would read as a distance
below."
  (and (>= position (window-start window))
       (- (align-buffer--pixels-between window (window-start window) position)
          (window-vscroll window t))))

(defun align-buffer--level-exactly (window position pixels)
  "Scroll WINDOW until POSITION is drawn PIXELS below its top.
Return non-nil when it had to move.

Measured again after each scroll rather than worked out once: one measurement
across a long stretch of a buffer can be wide of the mark, and a pass that
lands on the wrong row leaves the next one a short span to measure, where it
is not.  Bounded, since a pixel the window cannot draw POSITION at would
otherwise be chased for ever."
  (let ((tries 0)
        (drawn (align-buffer--drawn-at window position)))
    (while (and drawn (/= drawn pixels) (< tries 4))
      (align-buffer-scroll window (- drawn pixels))
      (setq tries (1+ tries))
      (setq drawn (align-buffer--drawn-at window position)))
    (> tries 0)))

(defun align-buffer--already-placed-p (window start point vscroll)
  "Return non-nil when WINDOW already begins at START, lifted VSCROLL, point
at POINT."
  (and (= (window-start window) start)
       (= (window-point window) point)
       (= (window-vscroll window t) vscroll)))


;;; Queueing a placement

(defun align-buffer--defer-place (session window side leader &optional delay)
  "Queue a placement of WINDOW, and arrange for it to happen.
DELAY waits that many seconds, which is how a placement waits for a redisplay
rather than merely for the command loop."
  (setq align-buffer--pending-place (list session window side leader))
  ;; A timer of no delay, which runs as soon as the command loop is free.
  ;; Waiting for the next command hook is not enough: a wheel notch over the
  ;; window the user is not in scrolls it, runs its command hook, and only THEN
  ;; reaches redisplay and us, so that hook has already been and gone.  The
  ;; placement would wait for the user's next keystroke, leaving the two a notch
  ;; apart for as long as they did nothing.
  (unless align-buffer--pending-timer
    (setq align-buffer--pending-timer
          (run-with-timer (or delay 0) nil
                          #'align-buffer--flush-pending-place))))

(defun align-buffer--drop-overtaken-place (window)
  "Drop a queued placement that WINDOW's lead has overtaken.

A queued placement carries the window that led when it was queued, and works
its target out afresh when it runs.  That is not enough on its own: by the
time it runs the lead may have changed hands, and then it places the window
the reader is now in, from a leader the reader has left - which moves their
point.  A newer lead says the queued one is out of date."
  (when (and align-buffer--pending-place
             (not (eq (nth 3 align-buffer--pending-place) window)))
    (setq align-buffer--pending-place nil)
    (when align-buffer--pending-timer
      (cancel-timer align-buffer--pending-timer)
      (setq align-buffer--pending-timer nil))))

(defun align-buffer-forget-pending-place (session)
  "Drop a queued placement of SESSION's, leaving another session's alone."
  (when (eq (nth 0 align-buffer--pending-place) session)
    (setq align-buffer--pending-place nil)
    (when align-buffer--pending-timer
      (cancel-timer align-buffer--pending-timer)
      (setq align-buffer--pending-timer nil))))

(defun align-buffer--flush-pending-place ()
  "Carry out a queued placement, from the timer set for it."
  (setq align-buffer--pending-timer nil)
  (align-buffer--apply-pending-place))

(defun align-buffer--apply-pending-place ()
  "Carry out a queued placement, if it is still the right thing to do."
  (when align-buffer--pending-place
    (let* ((pending align-buffer--pending-place)
           (session (nth 0 pending))
           (window (nth 1 pending))
           (side (nth 2 pending))
           (leader (nth 3 pending)))
      (setq align-buffer--pending-place nil)
      (when align-buffer--pending-timer
        (cancel-timer align-buffer--pending-timer)
        (setq align-buffer--pending-timer nil))
      (when (and (window-live-p window)
                 (window-live-p leader)
                 (align-buffer-session-live-p session)
                 (eq (window-buffer window) (align-buffer-buffer session side)))
        ;; Rule 1: the target is worked out again here, never carried along.
        ;; By now the leader has usually moved further, and a queued row would
        ;; leave the follower a notch behind and keep it there.
        (let ((align-buffer--syncing t))
          (align-buffer--place-now session window side leader))))))


;;; Syncing

(defconst align-buffer--least-window-height 4
  "Below this many rows a window is not treated as one of a session's.")

(defun align-buffer--window-big-enough-p (window)
  "Return non-nil when WINDOW is tall enough to take part."
  (and (window-live-p window)
       (>= (window-body-height window) align-buffer--least-window-height)))

(defun align-buffer--can-lead-p (window)
  "Return non-nil when WINDOW's viewport is worth following."
  ;; A buffer shown TWICE on one frame is one in the middle of being handed
  ;; around, and neither window is worth following: `display-buffer' makes its
  ;; window before it gives it a buffer, so for one redisplay the new window
  ;; shows what it was split from, with that window's start and a height of its
  ;; own.  The count is the signal because a window can only inherit a buffer
  ;; from a window that goes on showing it.
  (and (align-buffer--window-big-enough-p window)
       (= 1 (length (get-buffer-window-list (window-buffer window) nil
                                            (window-frame window))))))

(defun align-buffer--follower (session side)
  "Return the window showing the buffer facing SIDE, or nil."
  (let ((buffer (align-buffer-buffer session (align-buffer-other-side side))))
    (when (buffer-live-p buffer)
      (let ((windows (get-buffer-window-list buffer nil t)))
        (and (= (length windows) 1) (car windows))))))

(defun align-buffer-sync (window)
  "Align the window facing WINDOW's to WINDOW.
Return non-nil when the other window was moved.

Does nothing while `align-buffer-inhibit-sync' is non-nil, which is how a
consumer that is moving both windows itself keeps the correspondence from
pulling them back: what it is showing level may be something the pairs say
nothing about."
  (unless (or align-buffer--syncing align-buffer-inhibit-sync)
    (let ((session (align-buffer-session-for (window-buffer window)))
          (side (buffer-local-value 'align-buffer--side (window-buffer window))))
      (when (and session side (align-buffer--can-lead-p window))
        (align-buffer--drop-overtaken-place window)
        (let ((follower (align-buffer--follower session side)))
          (when (align-buffer--window-big-enough-p follower)
            (let ((align-buffer--syncing t))
              (align-buffer--place session follower
                                   (align-buffer-other-side side)
                                   window))))))))


;;; Triggers

(defun align-buffer--scroll-hook (window _start)
  "Sync from WINDOW as redisplay scrolls it."
  (when (window-live-p window)
    ;; Errors demoted, which turns a signal from the body into a message and
    ;; carries on.  Emacs REMOVES a hook function that signals, so one error
    ;; would leave the two unsynced for the rest of the session rather than for
    ;; one redisplay.
    (with-demoted-errors "align-buffer scroll sync: %S"
      (let ((ours (align-buffer--own-scroll-p window))
            (align-buffer--in-redisplay t))
        (unless ours
          (if (and align-buffer-inhibit-sync align-buffer-held-scroll-function)
              ;; A consumer holding both panes is the only one that can say
              ;; what this scroll means for them, and it is the one scroll it
              ;; cannot hear about any other way: redisplay moving the view
              ;; after the hook that placed them has been and gone.
              (funcall align-buffer-held-scroll-function window)
            (align-buffer-sync window)))))))

(defun align-buffer--level-here-p (window)
  "Return non-nil when the pane facing WINDOW already reads level with it.
Nil where the correspondence says nothing about the line point is on, which
is no promise to keep and so nothing to put right."
  (let* ((session (align-buffer-current-session))
         (side (buffer-local-value 'align-buffer--side (window-buffer window)))
         (follower (and session side (align-buffer--follower session side)))
         (target (and (window-live-p follower)
                      (align-buffer--target session side window follower))))
    (and target
         (eql (align-buffer--drawn-at follower (car target)) (cdr target)))))

(defvar align-buffer--wait-timer nil
  "The one-shot timer set to sync once a moving view has come to rest.")

(defun align-buffer--sync-when-settled (window)
  "Sync from WINDOW once the redisplay about to move it has been through.

A view the pairs are not read from yet still has to be read from: redisplay
brings point back by moving the start, which runs the scroll hook we would
otherwise wait for - but it can as well bring it back by clearing a lift,
which runs no hook at all.  A timer of no delay runs after that redisplay
either way."
  (unless align-buffer--wait-timer
    (setq align-buffer--wait-timer
          (run-with-timer
           0 nil
           (lambda ()
             (setq align-buffer--wait-timer nil)
             (when (window-live-p window)
               (with-demoted-errors "align-buffer settled sync: %S"
                 (with-current-buffer (window-buffer window)
                   (align-buffer-sync window)))))))))

(defun align-buffer--view-settled-p (window)
  "Return non-nil when WINDOW's view is one redisplay will leave alone.

Point outside what the window shows is a view about to move: redisplay has
yet to bring point back, and where it lands can take more than one pass.  The
pairs read from such a view speak for a view nobody ever sees, so the
placement waits for the scroll hook that the move runs.

Point outside counts as settled where the cursor is drawn by something other
than point: a consumer reading a row of its own keeps point on the line those
rows hang on, which the window may well not be showing, and that view is not
about to move anywhere."
  (or (pos-visible-in-window-p (point) window)
      (and align-buffer-point-for-scroll-function
           (funcall align-buffer-point-for-scroll-function window))))

(defun align-buffer--post-command ()
  "Sync after a command that scrolled or changed line, and force both paints."
  (with-demoted-errors "align-buffer post-command sync: %S"
    (align-buffer-keep-scrolled-view)
    (unless align-buffer--syncing
      (align-buffer--apply-pending-place)
      (let* ((window (selected-window))
             (start (window-start window))
             (lift (window-vscroll window t))
             (line (line-beginning-position))
             (tick (buffer-chars-modified-tick))
             ;; Entering the pane is a move of the reader's like any other,
             ;; and the one that changes which pair speaks for the two: level
             ;; on the line they have left is not level on the line they are
             ;; now on, so the facing pane is placed from here rather than
             ;; waiting for them to press something.
             (entered (and (not (eq window align-buffer--last-window))
                           (not (align-buffer--level-here-p window))))
             (ours (and (align-buffer--own-scroll-p window) (not entered))))
        ;; Same-line typing must cost nothing, hence the change test.  The
        ;; line beginning is watched as well as the start because a
        ;; point-motion scroll leaves the start stale here, and that is the
        ;; only signal that one is about to happen; and the lift because a
        ;; line that wraps is crossed a row at a time by the lift alone,
        ;; which moves the pair without touching either of the other two.
        ;;
        ;; Point itself is watched too, but only where the text has not
        ;; changed: typing moves point on every keystroke and must still cost
        ;; nothing, while a move of the reader's that leaves the line and the
        ;; view alone - a row at a time down a line that wraps, a column -
        ;; is a move the facing pane has to be placed from all the same, or
        ;; whatever it was left holding stays until something larger moves.
        (when (and (not ours)
                   (or entered
                       (null align-buffer--last-start)
                       (/= start align-buffer--last-start)
                       (null align-buffer--last-lift)
                       (/= lift align-buffer--last-lift)
                       (null align-buffer--last-line-beginning)
                       (/= line align-buffer--last-line-beginning)
                       (null align-buffer--last-point)
                       (and (eql tick align-buffer--last-tick)
                            (/= (point) align-buffer--last-point))))
          (let ((session (align-buffer-current-session)))
            (when (and session
                       (window-live-p (align-buffer--follower
                                       session align-buffer--side)))
              (redisplay t)))
          ;; Point off the view is a view about to move, and where it lands can
          ;; take more than one pass of redisplay: the end of a buffer is shown
          ;; with text above it rather than on the top row.  The pairs are read
          ;; from the view the reader is left with, so where point is still
          ;; outside it the placement waits for the scroll hook that move runs
          ;; rather than speaking for a view that was never on the screen.
          (unless (align-buffer--view-settled-p window)
            (align-buffer--sync-when-settled window))
          (when (align-buffer--view-settled-p window)
            ;; The rest is worth doing only when the follower actually moved
            ;; AND the command loop will not paint by itself, which it does
            ;; not while input waits: a held key, a burst of notches, a
            ;; keyboard macro.  For a single keystroke it paints every window
            ;; as soon as we return, and doing it ourselves costs two
            ;; redisplays and a visible change of face on both mode lines.  A
            ;; follower that did not move may still carry a mark from rule 3;
            ;; that waits for the end of the burst.
            (when (and (align-buffer-sync window)
                       (or (input-pending-p) executing-kbd-macro))
              (let ((session (align-buffer-current-session)))
                (when session
                  (let ((follower (align-buffer--follower
                                   session align-buffer--side)))
                    (when (window-live-p follower)
                      ;; The follower has to be SELECTED for its pixels to
                      ;; reach the screen: with input waiting Emacs paints
                      ;; only the selected window, and neither `redisplay' nor
                      ;; `force-window-update' overrides that.  Bound, or the
                      ;; follower would sync back onto the leader and drag its
                      ;; point along.
                      (let ((align-buffer--syncing t))
                        (with-selected-window follower (redisplay t)))
                      ;; Selecting the follower also paints its mode line in
                      ;; the ACTIVE face, and by the time redisplay next asks
                      ;; what changed the selection is back where it started,
                      ;; so it finds nothing to repair and both read as active.
                      (dolist (one (list window follower))
                        (with-current-buffer (window-buffer one)
                          (force-mode-line-update)))
                      (redisplay t))))))))
        (setq align-buffer--last-start (window-start window))
        (setq align-buffer--last-lift (window-vscroll window t))
        (setq align-buffer--last-line-beginning (line-beginning-position))
        (setq align-buffer--last-point (point))
        (setq align-buffer--last-tick (buffer-chars-modified-tick))
        (setq align-buffer--last-window window)))))

(defun align-buffer-sync-enable ()
  "Start syncing the current buffer."
  (add-hook 'window-scroll-functions #'align-buffer--scroll-hook nil t)
  (add-hook 'pre-command-hook #'align-buffer--forget-scrolled-view nil t)
  (add-hook 'post-command-hook #'align-buffer--post-command nil t)
  ;; We align in pixels, so a row at an edge of the window is drawn in part,
  ;; and the cursor sits on such a row whenever the pair we are level with
  ;; does.  Redisplay answers a cursor row drawn in part by throwing the lift
  ;; away and recentering (`cursor_row_fully_visible_p' in xdisp.c), which
  ;; loses the alignment and reads as a scroll of the reader's, handing that
  ;; window the lead.  `pixel-scroll-precision-mode' turns the same rule off
  ;; for the same reason.
  (setq align-buffer--cursor-line-setting
        (if (local-variable-p 'make-cursor-line-fully-visible)
            make-cursor-line-fully-visible
          'unset))
  (setq-local make-cursor-line-fully-visible nil))

(defun align-buffer-sync-disable ()
  "Stop syncing the current buffer."
  (remove-hook 'window-scroll-functions #'align-buffer--scroll-hook t)
  (remove-hook 'pre-command-hook #'align-buffer--forget-scrolled-view t)
  (remove-hook 'post-command-hook #'align-buffer--post-command t)
  (if (eq align-buffer--cursor-line-setting 'unset)
      (kill-local-variable 'make-cursor-line-fully-visible)
    (setq-local make-cursor-line-fully-visible
                align-buffer--cursor-line-setting))
  (kill-local-variable 'align-buffer--cursor-line-setting)
  (kill-local-variable 'align-buffer--last-start)
  (kill-local-variable 'align-buffer--last-lift)
  (kill-local-variable 'align-buffer--last-line-beginning))

(provide 'align-buffer-sync)

;;; align-buffer-sync.el ends here
