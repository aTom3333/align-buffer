;;; align-buffer-background.el --- A frame to run a harness in -*- lexical-binding: t; -*-

;;; Commentary:

;; A harness driving real input needs a frame redisplay actually draws.  An
;; invisible frame is not drawn at all: a window is never scrolled to its own
;; point, no scroll hook fires, and every reading taken is of a view nobody
;; would ever see.  A frame that is drawn but never focused and sits behind
;; whatever else is on the screen measures and behaves exactly as the reader's
;; own does.
;;
;; So Emacs is started with `--iconic', which keeps its own frame off the
;; screen, and the run makes the frame it uses with
;; `align-buffer-background-frame'.

;;; Code:

(defun align-buffer-background-frame (&optional columns rows)
  "Make, select and return the frame a harness runs in.
COLUMNS by ROWS, or maximized when they are not given.

Maximized is what a run that reads the screen wants, a frame hanging over the
edge of the screen being one the window system stops drawing, which leaves a
step waiting on a redisplay that never comes.  A size is for a run whose cost
grows with the window: every row of a window is somewhere a placement may have
to measure, and a walk of hundreds of steps over the tallest window there is
takes minutes a seed rather than seconds.

Either way it goes behind everything else and is never given the focus, so
that the keyboard and the screen stay the reader's while the run lasts."
  (let ((frame (make-frame (append (when (and columns rows)
                                     (list (cons 'width columns)
                                           (cons 'height rows)))
                                   '((no-focus-on-map . t)
                                     (no-accept-focus . t))))))
    (select-frame frame)
    ;; Maximized after the fact: the parameter is not honoured where a frame
    ;; is made with it.
    (unless (and columns rows)
      (set-frame-parameter frame 'fullscreen 'maximized))
    (lower-frame frame)
    (redisplay t)
    frame))

(provide 'align-buffer-background)

;;; align-buffer-background.el ends here
