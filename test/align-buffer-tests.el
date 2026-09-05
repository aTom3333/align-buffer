;;; align-buffer-tests.el --- Tests for align-buffer -*- lexical-binding: t; -*-

;;; Commentary:

;; The plan, the maps and the rendering, exercised with hand-written plans and no
;; generator.

;;; Code:

(require 'ert)
(require 'align-buffer)

(defun align-buffer-tests--source (name text &optional mode)
  "Return a buffer called NAME holding TEXT, under MODE."
  (let ((buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (insert text)
      (when mode (delay-mode-hooks (funcall mode)))
      (goto-char (point-min)))
    buffer))

(defun align-buffer-tests--line-cell (source line &optional face)
  (align-buffer-cell-create :kind 'line :source source :line line :face face))

(defun align-buffer-tests--blank-cell (&optional face)
  (align-buffer-cell-create :kind 'blank :face face))

(defun align-buffer-tests--text-cell (text)
  (align-buffer-cell-create :kind 'text :text text))

(defmacro align-buffer-tests--with-plan (plan-var &rest body)
  "Bind PLAN-VAR to a small two-source plan and run BODY."
  (declare (indent 1))
  `(let* ((old (align-buffer-tests--source " *old*" "alpha\nbeta\ndelta\n"))
          (new (align-buffer-tests--source " *new*" "alpha\ngamma\nbeta\ndelta\n"))
          (,plan-var
           (align-buffer-plan-create
            :left-name " *test left*"
            :right-name " *test right*"
            :rows
            (vector
             (align-buffer-row-create
              :left (align-buffer-tests--line-cell old 1)
              :right (align-buffer-tests--line-cell new 1))
             (align-buffer-row-create
              :left (align-buffer-tests--blank-cell 'shadow)
              :right (align-buffer-tests--line-cell new 2 'success)
              :tag 'added)
             (align-buffer-row-create
              :left (align-buffer-tests--line-cell old 2)
              :right (align-buffer-tests--line-cell new 3))
             (align-buffer-row-create
              :left (align-buffer-tests--line-cell old 3)
              :right (align-buffer-tests--line-cell new 4))))))
     (unwind-protect (progn ,@body)
       (kill-buffer old)
       (kill-buffer new))))

(defun align-buffer-tests--gutter-overlays ()
  "Return this pane's gutter overlays, earliest first."
  (seq-filter (lambda (overlay) (overlay-get overlay 'align-buffer-gutter))
              (sort (overlays-in (point-min) (point-max))
                    (lambda (one other)
                      (< (overlay-start one) (overlay-start other))))))

(defun align-buffer-tests--gutter-numbers (session side)
  "Return the number each of SESSION's rows shows in SIDE's gutter, or nil."
  (with-current-buffer (align-buffer-buffer session side)
    (mapcar (lambda (overlay)
              (let* ((strip (overlay-get overlay 'before-string))
                     (shown (and strip (cadr (get-text-property 0 'display strip)))))
                (and shown
                     (not (string-empty-p (string-trim shown)))
                     (string-trim shown))))
            (align-buffer-tests--gutter-overlays))))

(ert-deftest align-buffer-test-line-starts ()
  "Every line is indexed, the empty one after a final newline included."
  (let ((with-newline (align-buffer-tests--source " *starts*" "one\ntwo\n"))
        (without (align-buffer-tests--source " *starts bare*" "one\ntwo")))
    (unwind-protect
        (progn
          (should (equal (align-buffer--line-starts with-newline) [1 5 9]))
          (should (equal (align-buffer--line-starts without) [1 5])))
      (kill-buffer with-newline)
      (kill-buffer without))))

(ert-deftest align-buffer-test-copied-properties-are-filtered ()
  "We keep the face and drop everything else, `category' included."
  (let ((text (propertize "hello" 'face 'bold 'category 'some-category
                          'keymap (make-sparse-keymap))))
    (align-buffer--filter-properties text)
    (should (equal (get-text-property 0 'face text) 'bold))
    (should-not (get-text-property 0 'category text))
    (should-not (get-text-property 0 'keymap text))))

(ert-deftest align-buffer-test-pane-text ()
  "Each pane holds one real line per row, padding included."
  (align-buffer-tests--with-plan plan
    (let ((session (align-buffer-show plan)))
      (unwind-protect
          (progn
            (should (equal (with-current-buffer (align-buffer-buffer session 'left)
                             (buffer-substring-no-properties (point-min) (point-max)))
                           "alpha\n\nbeta\ndelta\n"))
            (should (equal (with-current-buffer (align-buffer-buffer session 'right)
                             (buffer-substring-no-properties (point-min) (point-max)))
                           "alpha\ngamma\nbeta\ndelta\n")))
        (align-buffer-quit session)))))

(ert-deftest align-buffer-test-row-is-the-pane-line ()
  "A row index is its pane line number minus one, both ways."
  (align-buffer-tests--with-plan plan
    (let ((session (align-buffer-show plan)))
      (unwind-protect
          (dolist (side '(left right))
            (with-current-buffer (align-buffer-buffer session side)
              (dotimes (row (align-buffer-row-count session))
                (let ((position (align-buffer-row-beginning-position session row side)))
                  (should (= (align-buffer-row-at-point position) row))
                  (goto-char position)
                  (should (= (line-number-at-pos) (1+ row)))))))
        (align-buffer-quit session)))))

(ert-deftest align-buffer-test-source-rows-are-one-to-many ()
  "A source line maps to every row showing it."
  (align-buffer-tests--with-plan plan
    (let* ((session (align-buffer-show plan))
           (new (align-buffer-cell-source
                 (align-buffer-cell session 0 'right))))
      (unwind-protect
          (progn
            (should (equal (align-buffer-source-rows session new 2)
                           '((1 . right))))
            (should-not (align-buffer-source-rows session new 99)))
        (align-buffer-quit session)))))

(ert-deftest align-buffer-test-repeated-source-line ()
  "The same source line shown twice maps to both rows."
  (let* ((source (align-buffer-tests--source " *twice*" "only\n"))
         (cell (align-buffer-tests--line-cell source 1))
         (plan (align-buffer-plan-create
                :left-name " *twice left*"
                :right-name " *twice right*"
                :rows (vector (align-buffer-row-create :left cell :right cell)
                              (align-buffer-row-create :left cell :right cell)))))
    (unwind-protect
        (let ((session (align-buffer-show plan)))
          (unwind-protect
              (should (equal (align-buffer-source-rows session source 1)
                             '((0 . left) (0 . right) (1 . left) (1 . right))))
            (align-buffer-quit session)))
      (kill-buffer source))))

(ert-deftest align-buffer-test-sections-from-tags ()
  "Adjacent rows sharing a tag are one section; untagged rows are in none."
  (let ((rows (vector (align-buffer-row-create :tag nil)
                      (align-buffer-row-create :tag 'change)
                      (align-buffer-row-create :tag 'change)
                      (align-buffer-row-create :tag nil)
                      (align-buffer-row-create :tag 'change))))
    (should (equal (align-buffer-derive-sections rows) [(1 . 2) (4 . 4)]))))

(ert-deftest align-buffer-test-visit-source-skips-padding ()
  "A padding row resolves to the nearest row above that has a source."
  (align-buffer-tests--with-plan plan
    (let ((session (align-buffer-show plan)))
      (unwind-protect
          (progn
            (should (= (align-buffer--nearest-source-row session 1 'left) 0))
            (should (= (align-buffer--nearest-source-row session 1 'right) 1)))
        (align-buffer-quit session)))))

(ert-deftest align-buffer-test-faces-come-from-the-source ()
  "A fontified source line brings its faces into the pane."
  (let* ((source (align-buffer-tests--source
                  " *faced*" ";; a comment\n(defun f ())\n" #'emacs-lisp-mode))
         (plan (align-buffer-plan-create
                :left-name " *faced left*"
                :right-name " *faced right*"
                :rows (vector (align-buffer-row-create
                               :left (align-buffer-tests--line-cell source 1)
                               :right (align-buffer-tests--text-cell "plain"))))))
    (unwind-protect
        (let ((session (align-buffer-show plan)))
          (unwind-protect
              (with-current-buffer (align-buffer-buffer session 'left)
                (should (get-char-property (point-min) 'face))
                (should-not (bound-and-true-p font-lock-mode)))
            (align-buffer-quit session)))
      (kill-buffer source))))

(ert-deftest align-buffer-test-parameters-from-buffer ()
  "Parameters carry values, not a buffer to read later."
  (let ((buffer (align-buffer-tests--source " *parameters*" "text\n")))
    (unwind-protect
        (with-current-buffer buffer
          (setq-local tab-width 3)
          (let ((parameters (align-buffer-parameters-from-buffer buffer)))
            (should (syntax-table-p (plist-get parameters :syntax-table)))
            (should (equal (alist-get 'tab-width (plist-get parameters :locals))
                           3))))
      (kill-buffer buffer))))

(ert-deftest align-buffer-test-quit-leaves-sources-alone ()
  "Tearing down kills the panes and nothing else."
  (align-buffer-tests--with-plan plan
    (let* ((session (align-buffer-show plan))
           (left (align-buffer-buffer session 'left))
           (source (align-buffer-cell-source (align-buffer-cell session 0 'left))))
      (align-buffer-quit session)
      (should-not (buffer-live-p left))
      (should (buffer-live-p source))
      (should-not (align-buffer-sessions)))))

(ert-deftest align-buffer-test-killing-a-pane-tears-down ()
  "Killing one pane out of band takes the session with it."
  (align-buffer-tests--with-plan plan
    (let* ((session (align-buffer-show plan))
           (right (align-buffer-buffer session 'right)))
      (kill-buffer (align-buffer-buffer session 'left))
      (should-not (buffer-live-p right))
      (should-not (align-buffer-sessions)))))

(ert-deftest align-buffer-test-rebuild-keeps-the-maps ()
  "Rebuilding from a new plan re-derives the positions."
  (align-buffer-tests--with-plan plan
    (let ((session (align-buffer-show plan)))
      (unwind-protect
          (let* ((rows (align-buffer-plan-rows plan))
                 (shorter (align-buffer-plan-create
                           :left-name (align-buffer-plan-left-name plan)
                           :right-name (align-buffer-plan-right-name plan)
                           :rows (vector (aref rows 0) (aref rows 2)))))
            (align-buffer-rebuild session shorter)
            (should (= (align-buffer-row-count session) 2))
            (should (equal (with-current-buffer (align-buffer-buffer session 'left)
                             (buffer-substring-no-properties (point-min) (point-max)))
                           "alpha\nbeta\n")))
        (align-buffer-quit session)))))

(ert-deftest align-buffer-test-next-section-takes-the-earliest-start ()
  "Forwards is the earliest start after the row, whatever the sections overlap."
  (let ((sections [(20 . 60) (25 . 30) (40 . 45)]))
    (should (= (align-buffer--section-after 0 sections) 0))
    (should (= (align-buffer--section-after 20 sections) 1))
    (should (= (align-buffer--section-after 25 sections) 2))
    (should-not (align-buffer--section-after 50 sections))))

(ert-deftest align-buffer-test-previous-section-takes-the-latest-start ()
  (let ((sections [(20 . 60) (25 . 30) (40 . 45)]))
    (should (= (align-buffer--section-before 50 sections) 2))
    (should (= (align-buffer--section-before 40 sections) 1))
    (should (= (align-buffer--section-before 25 sections) 0))
    (should-not (align-buffer--section-before 20 sections))))

(ert-deftest align-buffer-test-sections-sharing-a-start-are-visited-once ()
  "Where a block and a hunk begin together, one keypress is one move.
The first of them in the vector is the one visited, so a nested region does not
cost a keypress that appears to do nothing."
  (let ((sections [(20 . 60) (20 . 25)]))
    (should (= (align-buffer--section-after 0 sections) 0))
    (should-not (align-buffer--section-after 20 sections))))

(ert-deftest align-buffer-test-section-navigation-always-moves ()
  "Repeated moves never stand still, in a plan whose sections nest."
  (let* ((text (mapconcat #'number-to-string (number-sequence 1 80) "\n"))
         (old (align-buffer-tests--source " *nested old*" text))
         (new (align-buffer-tests--source " *nested new*" text))
         (plan (align-buffer-tests--big-plan old new 80)))
    (setf (align-buffer-plan-sections plan) '((20 . 60) (25 . 30) (40 . 45)))
    (unwind-protect
        (let ((session (align-buffer-show plan)))
          (unwind-protect
              (with-current-buffer (window-buffer (selected-window))
                (goto-char (point-min))
                (let ((visited nil))
                  (dotimes (_ 3)
                    (align-buffer-next-section)
                    (push (align-buffer-row-at-point) visited))
                  (should (equal (nreverse visited) '(20 25 40)))
                  (should-error (align-buffer-next-section) :type 'user-error)))
            (align-buffer-quit session)))
      (kill-buffer old)
      (kill-buffer new))))

(ert-deftest align-buffer-test-a-settled-viewport-is-left-alone ()
  "A window whose point clears its margins is already where it will stay."
  (should (= (align-buffer--settled-top-row 20 51 5 40) 20)))

(ert-deftest align-buffer-test-a-viewport-scrolls-to-clear-its-point ()
  "Point below the last safe row scrolls the window down by the difference.
The mouse wheel leaves point exactly there, and redisplay corrects it a moment
after we have synced, which is how the two panes ended up one row apart."
  (should (= (align-buffer--settled-top-row 17 51 5 63) 18))
  (should (= (align-buffer--settled-top-row 17 51 5 70) 25)))

(ert-deftest align-buffer-test-a-viewport-scrolls-back-for-a-point-above-it ()
  "A point above the first safe row pulls the top back far enough to clear it.
Point on row 10 with a 5-row margin needs a top of 5, not of 15: the margin is
counted from the top down to the point, not taken off the old top."
  (should (= (align-buffer--settled-top-row 20 51 5 10) 5))
  (should (= (align-buffer--settled-top-row 3 51 5 0) 0)))

(ert-deftest align-buffer-test-point-row-inside-the-window-is-left-alone ()
  "A row that already clears the margin is not moved."
  (should (= (align-buffer--clamp-point-row 30 20 54 5 79) 30)))

(ert-deftest align-buffer-test-point-row-clears-the-top-margin ()
  "A row inside the top margin moves down to the first safe row."
  (should (= (align-buffer--clamp-point-row 22 20 54 5 79) 25)))

(ert-deftest align-buffer-test-point-row-clears-the-bottom-margin ()
  "A row inside the bottom margin moves up to the last safe row."
  (should (= (align-buffer--clamp-point-row 73 20 54 5 79) 68)))

(ert-deftest align-buffer-test-point-row-above-a-shorter-follower ()
  "The leader's row can sit above a shorter follower, so it comes down to it.
With unequal heights the panes align on their centres, so a follower half the
leader's height shows none of the leader's top rows."
  (should (= (align-buffer--clamp-point-row 6 17 20 5 79) 22)))

(ert-deftest align-buffer-test-point-row-below-a-shorter-follower ()
  "A row below a shorter follower comes up to its last safe row.
The 20-row window at 17 shows rows 17 to 36, so a 5-row bottom margin leaves 31."
  (should (= (align-buffer--clamp-point-row 60 17 20 5 79) 31)))

(ert-deftest align-buffer-test-point-row-never-leaves-the-plan ()
  "Clamping stays inside the rows that exist, however small the plan."
  (should (= (align-buffer--clamp-point-row 3 0 54 5 2) 2))
  (should (= (align-buffer--clamp-point-row 5 0 54 5 2) 2)))

(ert-deftest align-buffer-test-point-row-at-the-top-of-the-plan ()
  "A viewport at row zero leaves the point wherever it is.
The margin exists to stop redisplay scrolling the window back, and there is
nothing above row zero to scroll into."
  (should (= (align-buffer--clamp-point-row 0 0 34 5 199) 0))
  (should (= (align-buffer--clamp-point-row 3 0 34 5 199) 3))
  (should (= (align-buffer--clamp-point-row 3 1 34 5 199) 6)))

(ert-deftest align-buffer-test-point-row-in-a-window-smaller-than-its-margins ()
  "A window too short for two margins prefers the top rather than inverting."
  (should (= (align-buffer--clamp-point-row 40 10 6 5 79) 15)))

(defun align-buffer-tests--big-plan (old new rows)
  "Return a plan of ROWS rows reading OLD and NEW line by line."
  (align-buffer-plan-create
   :left-name " *moving left*"
   :right-name " *moving right*"
   :rows (vconcat
          (cl-loop for line from 1 to rows
                   collect (align-buffer-row-create
                            :left (align-buffer-tests--line-cell old line)
                            :right (align-buffer-tests--line-cell new line))))))

(ert-deftest align-buffer-test-point-keeps-moving ()
  "Each command moves point one row and nothing puts it back.

Batch never runs `window-scroll-functions', so the hook is driven here the way
redisplay drives it: placing the follower changes its start, and if that scroll
is taken for the user's the follower leads from it, a placement of the user's own
window is queued, and the next command undoes the move.  That is a frozen pane."
  (let* ((text (mapconcat (lambda (n) (format "line %d" n))
                          (number-sequence 1 60) "\n"))
         (old (align-buffer-tests--source " *moving old*" text))
         (new (align-buffer-tests--source " *moving new*" text)))
    (unwind-protect
        (let ((session (align-buffer-show
                        (align-buffer-tests--big-plan old new 60))))
          (unwind-protect
              (with-current-buffer (window-buffer (selected-window))
                (let ((follower (get-buffer-window
                                 (align-buffer-buffer
                                  session (align-buffer-other-side
                                           align-buffer--side))))
                      (rows nil))
                  (goto-char (align-buffer-row-beginning-position session 20
                                                        align-buffer--side))
                  (dotimes (_ 5)
                    (forward-line 1)
                    (align-buffer--post-command)
                    (align-buffer--scroll-hook follower (window-start follower))
                    (align-buffer--post-command)
                    (push (align-buffer-row-at-point) rows))
                  (should (equal (nreverse rows) '(21 22 23 24 25)))))
            (align-buffer-quit session)))
      (kill-buffer old)
      (kill-buffer new))))

(ert-deftest align-buffer-test-a-tiny-window-does-not-lead ()
  "A one-line window showing a pane never leads a sync.
Splitting a pane's window leaves the new window showing the pane's buffer for an
instant, as hydra's hint window does.  Its start is real but its height is not
the pane's, so the centre computed from it puts the other pane elsewhere."
  (let* ((text (mapconcat #'number-to-string (number-sequence 1 60) "\n"))
         (old (align-buffer-tests--source " *tiny old*" text))
         (new (align-buffer-tests--source " *tiny new*" text)))
    (unwind-protect
        (let ((session (align-buffer-show
                        (align-buffer-tests--big-plan old new 60))))
          (unwind-protect
              (let* ((leader (get-buffer-window (align-buffer-buffer session 'left)))
                     (follower (get-buffer-window
                                (align-buffer-buffer session 'right)))
                     (before (with-current-buffer (window-buffer follower)
                               (align-buffer-row-at-point
                                (window-start follower)))))
                (set-window-start leader (align-buffer-row-beginning-position session 30 'left))
                (align-buffer--set-point
                 leader (align-buffer-row-beginning-position session 30 'left))
                (cl-letf (((symbol-function 'window-body-height)
                           (lambda (&rest _) 1)))
                  (align-buffer-sync leader))
                (should (= (with-current-buffer (window-buffer follower)
                             (align-buffer-row-at-point (window-start follower)))
                           before)))
            (align-buffer-quit session)))
      (kill-buffer old)
      (kill-buffer new))))

(ert-deftest align-buffer-test-our-own-scroll-does-not-lead ()
  "A start we set is recognised as ours by everyone who asks, until it moves.

Both the scroll hook and the command hook ask this about the same window in the
same cycle, so the answer has to be stable rather than consumed by whoever asks
first.  It stops being ours once the window is somewhere else, which is what
makes a later scroll back to the same row the user's own."
  (let* ((text (mapconcat #'number-to-string (number-sequence 1 40) "\n"))
         (old (align-buffer-tests--source " *own old*" text))
         (new (align-buffer-tests--source " *own new*" text)))
    (unwind-protect
        (let ((session (align-buffer-show
                        (align-buffer-tests--big-plan old new 40))))
          (unwind-protect
              (let* ((window (get-buffer-window
                              (align-buffer-buffer session 'left)))
                     (start (align-buffer-row-beginning-position session 3 'left)))
                (set-window-start window start)
                (align-buffer--note-placed window start (window-point window))
                (should (align-buffer--own-scroll-p window))
                (should (align-buffer--own-scroll-p window))
                (set-window-start window
                                  (align-buffer-row-beginning-position
                                   session 9 'left))
                (should-not (align-buffer--own-scroll-p window))
                (set-window-start window start)
                (should-not (align-buffer--own-scroll-p window)))
            (align-buffer-quit session)))
      (kill-buffer old)
      (kill-buffer new))))

(ert-deftest align-buffer-test-one-side-reads-two-buffers ()
  "A side may take its rows from more than one source buffer."
  (let* ((one (align-buffer-tests--source " *one*" "from one\n"))
         (two (align-buffer-tests--source " *two*" "from two\n"))
         (plan (align-buffer-plan-create
                :left-name " *mixed left*"
                :right-name " *mixed right*"
                :rows (vector (align-buffer-row-create
                               :left (align-buffer-tests--line-cell one 1)
                               :right (align-buffer-tests--blank-cell))
                              (align-buffer-row-create
                               :left (align-buffer-tests--line-cell two 1)
                               :right (align-buffer-tests--blank-cell))))))
    (unwind-protect
        (let ((session (align-buffer-show plan)))
          (unwind-protect
              (progn
                (should (equal (with-current-buffer
                                   (align-buffer-buffer session 'left)
                                 (buffer-substring-no-properties
                                  (point-min) (point-max)))
                               "from one\nfrom two\n"))
                (should (equal (align-buffer-sources session 'left)
                               (list one two)))
                (should (equal (align-buffer-source-rows session two 1)
                               '((1 . left)))))
            (align-buffer-quit session)))
      (kill-buffer one)
      (kill-buffer two))))

(ert-deftest align-buffer-test-hooks-run ()
  "The layout, build and teardown hooks run with what they promise."
  (align-buffer-tests--with-plan plan
    (let* ((seen nil)
           (align-buffer-post-build-functions
            (list (lambda (session) (push (cons 'build session) seen))))
           (align-buffer-post-layout-functions
            (list (lambda (session windows)
                    (push (list 'layout session (length windows)) seen))))
           (align-buffer-pre-teardown-functions
            (list (lambda (session) (push (cons 'teardown session) seen))))
           (session (align-buffer-show plan)))
      (align-buffer-quit session)
      (should (equal (mapcar #'car (reverse seen)) '(build layout teardown)))
      (should (equal (nth 2 (assq 'layout seen)) 2)))))

(ert-deftest align-buffer-test-quit-collapses-onto-the-window-a-hook-left ()
  "Quit collapses onto whichever pane window its teardown hook left on screen.
A consumer grouping the panes deletes one of them from that hook, and the other
may then be the only ordinary window on the frame."
  (align-buffer-tests--with-plan plan
    (delete-other-windows)
    (let* ((align-buffer-pre-teardown-functions
            (list (lambda (session)
                    (delete-window (get-buffer-window
                                    (align-buffer-buffer session 'left))))))
           (session (align-buffer-show plan))
           (survivor (get-buffer-window (align-buffer-buffer session 'right))))
      (align-buffer-quit session)
      (should (window-live-p survivor))
      (should (equal (window-list nil 'no-mini) (list survivor))))))

(ert-deftest align-buffer-test-quit-spares-a-window-that-holds-no-pane ()
  "Quit deletes the pane windows its teardown hook left, and nothing besides.
The place the panes held stays on the frame, showing what it falls back to."
  (align-buffer-tests--with-plan plan
    (delete-other-windows)
    (let* ((other (split-window nil nil 'below))
           (align-buffer-pre-teardown-functions
            (list (lambda (session)
                    (delete-window (get-buffer-window
                                    (align-buffer-buffer session 'left))))))
           (session (align-buffer-show plan))
           (survivor (get-buffer-window (align-buffer-buffer session 'right))))
      (align-buffer-quit session)
      (should (window-live-p other))
      (should (window-live-p survivor))
      (should (= (length (window-list nil 'no-mini)) 2)))))

(ert-deftest align-buffer-test-rebuild-lets-a-consumer-redraw ()
  "A rebuild runs the build and layout hooks, and spares what a consumer drew.
Rendering clears the panes, so a decoration is gone and its owner needs both the
news and its overlay left alone until then."
  (align-buffer-tests--with-plan plan
    (let* ((seen nil)
           (theirs nil)
           (align-buffer-post-build-functions
            (list (lambda (session)
                    (push 'build seen)
                    (with-current-buffer (align-buffer-buffer session 'left)
                      (setq theirs (make-overlay (point-min) (point-max)))
                      (overlay-put theirs 'consumer-decoration t)))))
           (align-buffer-post-layout-functions
            (list (lambda (_session windows) (push (length windows) seen))))
           (session (align-buffer-show plan)))
      (unwind-protect
          (progn
            (setq seen nil)
            (align-buffer-rebuild session (align-buffer-session-plan session))
            (should (equal (reverse seen) '(build 2)))
            (should (overlay-buffer theirs))
            (should (overlay-get theirs 'consumer-decoration)))
        (align-buffer-quit session)))))

(ert-deftest align-buffer-test-a-render-that-signals-leaves-nothing-behind ()
  "A plan that cannot be rendered takes its half-built panes with it."
  (let* ((before (length (buffer-list)))
         (plan (align-buffer-plan-create
                :left-name " *doomed left*"
                :right-name " *doomed right*"
                :rows (vector nil))))
    (should-error (align-buffer-show plan))
    (should (null align-buffer--sessions))
    (should-not (get-buffer " *doomed left*"))
    (should-not (get-buffer " *doomed right*"))
    (should (= (length (buffer-list)) before))))

(ert-deftest align-buffer-test-a-plan-can-own-buffers ()
  "Buffers the plan names as its own are killed with the session.
A generator that made a buffer to read from - a revision, a scratch source -
says so on the plan rather than tearing it down itself."
  (align-buffer-tests--with-plan plan
    (let ((owned (generate-new-buffer " *owned source*")))
      (setf (align-buffer-plan-properties plan) (list :owned-buffers (list owned)))
      (align-buffer-quit (align-buffer-show plan))
      (should-not (buffer-live-p owned)))))

(ert-deftest align-buffer-test-an-owned-buffer-with-edits-survives ()
  "A buffer the reader has edited is theirs, whoever named it."
  (align-buffer-tests--with-plan plan
    (let ((owned (generate-new-buffer " *owned and edited*")))
      (with-current-buffer owned (insert "the reader typed this"))
      (setf (align-buffer-plan-properties plan) (list :owned-buffers (list owned)))
      (align-buffer-quit (align-buffer-show plan))
      (should (buffer-live-p owned))
      (kill-buffer owned))))

(ert-deftest align-buffer-test-quit-forgets-the-placement-record ()
  "Teardown clears the record we kept on the windows.
Left behind, the next session laid out in that window reads it as its own and
skips the reader's first move there."
  (align-buffer-tests--with-plan plan
    (let* ((session (align-buffer-show plan))
           (window (get-buffer-window (align-buffer-buffer session 'left))))
      (align-buffer--note-placed window (window-start window) (window-point window))
      (should (window-parameter window 'align-buffer-placed))
      (align-buffer-quit session)
      (should-not (window-parameter window 'align-buffer-placed)))))

(ert-deftest align-buffer-test-one-session-does-not-drop-another-placement ()
  "Tearing a session down leaves another session's queued placement alone."
  (align-buffer-tests--with-plan first-plan
    (align-buffer-tests--with-plan second-plan
      (let* ((one (align-buffer-show first-plan))
             (two (align-buffer-show second-plan))
             (queued (list two nil 'left 3 nil)))
        (unwind-protect
            (progn
              (setq align-buffer--pending-place queued)
              (align-buffer-quit one)
              (should (eq align-buffer--pending-place queued))
              (align-buffer-forget-pending-place two)
              (should-not align-buffer--pending-place))
          (align-buffer-quit two))))))

(ert-deftest align-buffer-test-refinement-covers-the-ranges-it-names ()
  "A cell's refinement paints its ranges, over the row's own face.
The ranges are character offsets into the row's text, which is what a generator
that knows the intra-line difference can supply."
  (let* ((source (align-buffer-tests--source " *refined*" "int foo(int bar);
"))
         (plan (align-buffer-plan-create
                :left-name " *refined left*"
                :right-name " *refined right*"
                :rows (vector
                       (align-buffer-row-create
                        :left (align-buffer-cell-create
                               :kind 'line :source source :line 1 :face 'shadow
                               :refinement '((4 7 success) (12 15 warning)))
                        :right (align-buffer-tests--blank-cell))))))
    (unwind-protect
        (let ((session (align-buffer-show plan)))
          (unwind-protect
              (with-current-buffer (align-buffer-buffer session 'left)
                (let ((refinements
                       (sort (seq-filter
                              (lambda (overlay)
                                (overlay-get overlay 'align-buffer-refinement))
                              (overlays-in (point-min) (point-max)))
                             (lambda (one other)
                               (< (overlay-start one) (overlay-start other))))))
                  (should (equal (mapcar (lambda (overlay)
                                           (list (buffer-substring-no-properties
                                                  (overlay-start overlay)
                                                  (overlay-end overlay))
                                                 (overlay-get overlay 'face)))
                                         refinements)
                                 '(("foo" success) ("bar" warning))))
                  ;; Above the row face, or the row's colour would hide them.
                  (should (cl-every (lambda (overlay)
                                      (> (or (overlay-get overlay 'priority) 0) 0))
                                    refinements))))
            (align-buffer-quit session)))
      (kill-buffer source))))

(ert-deftest align-buffer-test-refinement-stops-at-the-row-end ()
  "A range reaching past the line stops at it rather than covering the newline."
  (let* ((source (align-buffer-tests--source " *short*" "ab
"))
         (plan (align-buffer-plan-create
                :left-name " *short left*"
                :right-name " *short right*"
                :rows (vector
                       (align-buffer-row-create
                        :left (align-buffer-cell-create
                               :kind 'line :source source :line 1
                               :refinement '((0 40 success)))
                        :right (align-buffer-tests--blank-cell))))))
    (unwind-protect
        (let ((session (align-buffer-show plan)))
          (unwind-protect
              (with-current-buffer (align-buffer-buffer session 'left)
                (let ((overlay (car (seq-filter
                                     (lambda (overlay)
                                       (overlay-get overlay 'align-buffer-refinement))
                                     (overlays-in (point-min) (point-max))))))
                  (should (equal (buffer-substring-no-properties
                                  (overlay-start overlay) (overlay-end overlay))
                                 "ab"))))
            (align-buffer-quit session)))
      (kill-buffer source))))

(ert-deftest align-buffer-test-faces-can-be-copied-again ()
  "A face the source gains after the build reaches the pane on a refresh.
Which is what a language server's semantic tokens need: they arrive well after
the panes were rendered."
  (align-buffer-tests--with-plan plan
    (let ((session (align-buffer-show plan)))
      (unwind-protect
          (progn
            (should (null (with-current-buffer (align-buffer-buffer session 'left)
                            (get-text-property (point-min) 'face))))
            (with-current-buffer old
              (put-text-property (point-min) (+ (point-min) 5) 'face 'success))
            (should (= (align-buffer-refresh-faces session old) 3))
            (should (eq (with-current-buffer (align-buffer-buffer session 'left)
                          (get-text-property (point-min) 'face))
                        'success)))
        (align-buffer-quit session)))))

(ert-deftest align-buffer-test-copying-again-keeps-to-one-source ()
  "A refresh touches only the rows that read the buffer it was given."
  (align-buffer-tests--with-plan plan
    (let ((session (align-buffer-show plan)))
      (unwind-protect
          (progn
            (with-current-buffer new
              (put-text-property (point-min) (+ (point-min) 5) 'face 'success))
            ;; The right pane reads `new' on all four rows, the left never.
            (should (= (align-buffer-refresh-faces session new) 4))
            (should (null (with-current-buffer (align-buffer-buffer session 'left)
                            (get-text-property (point-min) 'face)))))
        (align-buffer-quit session)))))

(ert-deftest align-buffer-test-copying-again-can-take-a-line-range ()
  "A refresh limited to a line range leaves the rows outside it alone."
  (align-buffer-tests--with-plan plan
    (let ((session (align-buffer-show plan)))
      (unwind-protect
          (progn
            (with-current-buffer old
              (put-text-property (point-min) (point-max) 'face 'success))
            (should (= (align-buffer-refresh-faces session old 2 3) 2))
            ;; Row zero reads line one, which the range left out.
            (should (null (with-current-buffer (align-buffer-buffer session 'left)
                            (get-text-property (point-min) 'face)))))
        (align-buffer-quit session)))))

(ert-deftest align-buffer-test-copying-again-skips-a-line-of-another-length ()
  "A source line that has grown or shrunk means a stale plan, so it is skipped."
  (align-buffer-tests--with-plan plan
    (let ((session (align-buffer-show plan)))
      (unwind-protect
          (progn
            (with-current-buffer old
              (goto-char (point-min))
              (insert "much longer now ")
              (put-text-property (point-min) (+ (point-min) 5) 'face 'success))
            (should (= (align-buffer-refresh-faces session old 1 1) 0)))
        (align-buffer-quit session)))))

(ert-deftest align-buffer-test-copying-again-skips-a-line-that-reads-otherwise ()
  "A source line rewritten to the same length is skipped too.
Its faces describe text the pane does not show, so painting them would colour
whatever happens to sit at those offsets."
  (align-buffer-tests--with-plan plan
    (let ((session (align-buffer-show plan)))
      (unwind-protect
          (progn
            (with-current-buffer old
              ;; "alpha" becomes "ALPHA", same length, every character different.
              (goto-char (point-min))
              (delete-region (point-min) (+ (point-min) 5))
              (insert "ALPHA")
              (put-text-property (point-min) (+ (point-min) 5) 'face 'success))
            (should (= (align-buffer-refresh-faces session old 1 1) 0))
            (should (null (with-current-buffer (align-buffer-buffer session 'left)
                            (get-text-property (point-min) 'face))))
            ;; And the pane still shows what the plan was made from.
            (should (equal (with-current-buffer (align-buffer-buffer session 'left)
                             (buffer-substring-no-properties
                              (point-min) (+ (point-min) 5)))
                           "alpha")))
        (align-buffer-quit session)))))

(ert-deftest align-buffer-test-copying-again-keeps-the-overlays ()
  "A refresh replaces properties, not text, so the gutter survives it."
  (align-buffer-tests--with-plan plan
    (let ((session (align-buffer-show plan)))
      (unwind-protect
          (with-current-buffer (align-buffer-buffer session 'left)
            (let ((before (length (align-buffer-tests--gutter-overlays))))
              (align-buffer-refresh-faces session old)
              (should (= (length (align-buffer-tests--gutter-overlays)) before))
              (should-not (buffer-modified-p))))
        (align-buffer-quit session)))))

(ert-deftest align-buffer-test-gutter-width ()
  "The gutter is as wide as its largest number, and absent without numbers."
  (align-buffer-tests--with-plan plan
    (let ((rows (align-buffer-plan-rows plan)))
      (should (= (align-buffer--gutter-width rows 'left) 3))
      (should (= (align-buffer--gutter-width
                 (vector (align-buffer-row-create
                          :left (align-buffer-tests--blank-cell)
                          :right (align-buffer-tests--text-cell "no number")))
                 'right)
                0)))))

(ert-deftest align-buffer-test-gutter-can-be-turned-off ()
  "With the gutter off a pane has no margin and no gutter overlay."
  (align-buffer-tests--with-plan plan
    (let* ((align-buffer-show-gutter nil)
           (session (align-buffer-show plan)))
      (unwind-protect
          (with-current-buffer (align-buffer-buffer session 'left)
            (should (= (align-buffer-session-left-gutter-width session) 0))
            (should-not left-margin-width)
            (should (null (align-buffer-tests--gutter-overlays))))
        (align-buffer-quit session)))))

(ert-deftest align-buffer-test-gutter-shows-the-source-line-numbers ()
  "Each row carries a margin strip: its source line number, or blank padding.
The strip is what the reader sees in place of native line numbers, which count
pane lines rather than source lines."
  (align-buffer-tests--with-plan plan
    (let ((session (align-buffer-show plan)))
      (unwind-protect
          (with-current-buffer (align-buffer-buffer session 'left)
            (should (= left-margin-width
                       (align-buffer-session-left-gutter-width session)))
            (should (= (length (align-buffer-tests--gutter-overlays))
                       (align-buffer-row-count session)))
            (should (equal (align-buffer-tests--gutter-numbers session 'left)
                           '("1" nil "2" "3"))))
        (align-buffer-quit session)))))

(ert-deftest align-buffer-test-refresh-keeps-the-viewport ()
  "Rebuilding from the same plan leaves the panes where they were."
  (let* ((text (mapconcat #'number-to-string (number-sequence 1 60) "\n"))
         (old (align-buffer-tests--source " *refresh old*" text))
         (new (align-buffer-tests--source " *refresh new*" text)))
    (unwind-protect
        (let ((session (align-buffer-show
                        (align-buffer-tests--big-plan old new 60))))
          (unwind-protect
              (let ((window (get-buffer-window
                             (align-buffer-buffer session 'left))))
                (set-window-start window
                                  (align-buffer-row-beginning-position session 12 'left))
                (align-buffer-rebuild session (align-buffer-session-plan session))
                (should (= (with-current-buffer (window-buffer window)
                             (align-buffer-row-at-point (window-start window)))
                           12)))
            (align-buffer-quit session)))
      (kill-buffer old)
      (kill-buffer new))))

(ert-deftest align-buffer-test-quit-collapses-to-one-window ()
  "Quitting leaves a single window showing something other than a pane."
  (align-buffer-tests--with-plan plan
    (let ((before (length (window-list)))
          (session (align-buffer-show plan)))
      (should (= (length (window-list)) (1+ before)))
      (align-buffer-quit session)
      (should (= (length (window-list)) before))
      (should-not (align-buffer-current-session)))))

(ert-deftest align-buffer-test-sync-survives-a-rebuild ()
  "Both panes carry the sync triggers, after a build and after a rebuild.
Entering the pane's major mode clears buffer-local hooks, so a rebuild that does
not put them back leaves only the scroll hook, which cannot cover the cases the
command hook does."
  (align-buffer-tests--with-plan plan
    (let ((session (align-buffer-show plan)))
      (unwind-protect
          (dolist (stage '(built rebuilt))
            (when (eq stage 'rebuilt)
              (align-buffer-rebuild session (align-buffer-session-plan session)))
            (dolist (side '(left right))
              (with-current-buffer (align-buffer-buffer session side)
                (should (memq #'align-buffer--scroll-hook window-scroll-functions))
                (should (memq #'align-buffer--post-command post-command-hook))
                (should (local-variable-p 'window-scroll-functions))
                (should (local-variable-p 'post-command-hook)))))
        (align-buffer-quit session)))))

(ert-deftest align-buffer-test-an-unrelated-buffer-of-that-name-is-untouched ()
  "We never take over a buffer we do not own, and never erase it.
Rendering erases what it is given with undo off, so taking over a file's buffer
would destroy it, and a generator naming panes after paths will meet one."
  (let ((bystander (align-buffer-tests--source " *taken*" "precious\n")))
    (unwind-protect
        (let* ((plan (align-buffer-plan-create
                      :left-name " *taken*"
                      :right-name " *taken right*"
                      :rows (vector (align-buffer-row-create))))
               (session (align-buffer-show plan)))
          (unwind-protect
              (progn
                (should-not (eq (align-buffer-buffer session 'left) bystander))
                (should (buffer-live-p bystander))
                (should (equal (with-current-buffer bystander (buffer-string))
                               "precious\n")))
            (align-buffer-quit session)))
      (kill-buffer bystander))))

(ert-deftest align-buffer-test-a-pane-may-be-off-screen ()
  "A session with only one pane displayed stays alive, and syncing is a no-op.
Where the panes go is the user's to arrange, so anything may take a pane's
window; we look windows up by buffer each time and do nothing when one is gone."
  (let* ((text (mapconcat #'number-to-string (number-sequence 1 40) "\n"))
         (old (align-buffer-tests--source " *offscreen old*" text))
         (new (align-buffer-tests--source " *offscreen new*" text))
         (elsewhere (align-buffer-tests--source " *elsewhere*" "other\n")))
    (unwind-protect
        (let ((session (align-buffer-show
                        (align-buffer-tests--big-plan old new 40))))
          (unwind-protect
              (let* ((left (align-buffer-buffer session 'left))
                     (window (get-buffer-window left))
                     (survivor (get-buffer-window
                                (align-buffer-buffer session 'right))))
                (set-window-buffer window elsewhere)
                (should (align-buffer-session-live-p session))
                (should-not (get-buffer-window left))
                (let ((before (window-start window)))
                  (align-buffer-sync survivor)
                  (should (= (window-start window) before))
                  (should (eq (window-buffer window) elsewhere)))
                (set-window-buffer window left)
                (set-window-start survivor
                                  (align-buffer-row-beginning-position session 10 'right))
                (set-window-point survivor
                                  (align-buffer-row-beginning-position session 20 'right))
                (align-buffer-sync survivor)
                (should (= (with-current-buffer left
                             (align-buffer-row-at-point (window-start window)))
                           10)))
            (align-buffer-quit session)))
      (kill-buffer old)
      (kill-buffer new)
      (kill-buffer elsewhere))))

(ert-deftest align-buffer-test-two-sessions-do-not-share-panes ()
  "Two plans asking for the same pane name get a pane each."
  (align-buffer-tests--with-plan first-plan
    (align-buffer-tests--with-plan second-plan
      (let* ((first (align-buffer-show first-plan))
             (second (align-buffer-show second-plan)))
        (unwind-protect
            (progn
              (should-not (eq (align-buffer-buffer first 'left)
                              (align-buffer-buffer second 'left)))
              (align-buffer-quit second)
              (should (align-buffer-session-live-p first)))
          (when (align-buffer-session-live-p first) (align-buffer-quit first)))))))

(ert-deftest align-buffer-test-quit-restores-the-buffer-underneath ()
  "The surviving window goes back to what it showed, not one step further."
  (let ((underneath (align-buffer-tests--source "*align-buffer underneath*"
                                                "text\n")))
    (unwind-protect
        (align-buffer-tests--with-plan plan
          (switch-to-buffer underneath)
          (let ((session (align-buffer-show plan)))
            (align-buffer-quit session)
            (should (eq (window-buffer (selected-window)) underneath))))
      (kill-buffer underneath))))

(ert-deftest align-buffer-test-explicit-sections-are-usable ()
  "A plan may hand us its sections as a list, as its docstring says."
  (align-buffer-tests--with-plan plan
    (setf (align-buffer-plan-sections plan) '((1 . 2)))
    (let ((session (align-buffer-show plan)))
      (unwind-protect
          (progn
            (should (equal (align-buffer-session-sections session) [(1 . 2)]))
            (with-current-buffer (align-buffer-buffer session 'right)
              (goto-char (point-min))
              (align-buffer-next-section)
              (should (= (align-buffer-row-at-point) 1))))
        (align-buffer-quit session)))))

(ert-deftest align-buffer-test-visit-source-refuses-a-text-row ()
  "Text a generator supplied stands for no line of any buffer."
  (let* ((source (align-buffer-tests--source " *refuses*" "real\n"))
         (plan (align-buffer-plan-create
                :left-name " *refuses left*"
                :right-name " *refuses right*"
                :rows (vector (align-buffer-row-create
                               :left (align-buffer-tests--line-cell source 1)
                               :right (align-buffer-tests--text-cell "invented"))))))
    (unwind-protect
        (let ((session (align-buffer-show plan)))
          (unwind-protect
              (with-current-buffer (align-buffer-buffer session 'right)
                (goto-char (point-min))
                (should-error (align-buffer-visit-source) :type 'user-error))
            (align-buffer-quit session)))
      (kill-buffer source))))

(ert-deftest align-buffer-test-text-cells-render-their-own-text ()
  (let* ((plan (align-buffer-plan-create
                :left-name " *text left*"
                :right-name " *text right*"
                :rows (vector (align-buffer-row-create
                               :left (align-buffer-tests--text-cell "invented")
                               :right (align-buffer-tests--blank-cell)))))
         (session (align-buffer-show plan)))
    (unwind-protect
        (should (equal (with-current-buffer (align-buffer-buffer session 'left)
                         (buffer-substring-no-properties (point-min) (point-max)))
                       "invented\n"))
      (align-buffer-quit session))))

(ert-deftest align-buffer-test-copied-face-matches-the-source ()
  "The pane's face at a position is the one the source has there."
  (let* ((source (align-buffer-tests--source
                  " *matching*" ";; comment\n" #'emacs-lisp-mode))
         (plan (align-buffer-plan-create
                :left-name " *matching left*"
                :right-name " *matching right*"
                :rows (vector (align-buffer-row-create
                               :left (align-buffer-tests--line-cell source 1)
                               :right (align-buffer-tests--blank-cell))))))
    (unwind-protect
        (let* ((session (align-buffer-show plan))
               (wanted (with-current-buffer source
                         (get-text-property (point-min) 'face))))
          (should wanted)
          (unwind-protect
              (with-current-buffer (align-buffer-buffer session 'left)
                (should (equal (get-char-property (point-min) 'face) wanted)))
            (align-buffer-quit session)))
      (kill-buffer source))))

(ert-deftest align-buffer-test-teardown-runs-once ()
  "Killing a pane during teardown does not tear the session down twice."
  (align-buffer-tests--with-plan plan
    (let* ((runs 0)
           (align-buffer-pre-teardown-functions
            (list (lambda (_session) (setq runs (1+ runs)))))
           (session (align-buffer-show plan)))
      (kill-buffer (align-buffer-buffer session 'left))
      (should (= runs 1))
      (should-not (align-buffer-sessions)))))

(ert-deftest align-buffer-test-a-dead-session-leaves-the-registry ()
  "A session whose panes went away is forgotten, not merely filtered out."
  (align-buffer-tests--with-plan plan
    (let ((session (align-buffer-show plan)))
      (let ((align-buffer--tearing-down t))
        (dolist (side '(left right))
          (kill-buffer (align-buffer-buffer session side))))
      (should-not (align-buffer-sessions))
      (should-not (memq session align-buffer--sessions)))))

(ert-deftest align-buffer-test-row-at-point-agrees-with-counting-lines ()
  "The searched row and the counted line agree, at every row and both edges."
  (let* ((text (mapconcat #'number-to-string (number-sequence 1 40) "\n"))
         (old (align-buffer-tests--source " *search old*" text))
         (new (align-buffer-tests--source " *search new*" text)))
    (unwind-protect
        (let ((session (align-buffer-show
                        (align-buffer-tests--big-plan old new 40))))
          (unwind-protect
              (with-current-buffer (align-buffer-buffer session 'left)
                (dotimes (row 40)
                  (let ((start (align-buffer-row-beginning-position session row 'left)))
                    (should (= (align-buffer-row-at-point start) row))
                    (should (= (align-buffer-row-at-point
                                (align-buffer--row-end session row 'left))
                               row))))
                (should (= (align-buffer-row-at-point (point-min)) 0)))
            (align-buffer-quit session)))
      (kill-buffer old)
      (kill-buffer new))))

(provide 'align-buffer-tests)

;;; align-buffer-tests.el ends here
