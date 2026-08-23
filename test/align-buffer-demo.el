;;; align-buffer-demo.el --- A hand-written plan to look at -*- lexical-binding: t; -*-

;;; Commentary:

;; `align-buffer-demo' builds two source buffers and a plan by hand, so the core
;; can be exercised without any generator: the gutter, the scroll sync, section
;; navigation with C-j and C-k, RET out to the source, and q to quit.
;;
;; The right side gains a block of lines the left does not have, so the left
;; pads; the left has a block the right does not, so the right pads.  Every
;; twentieth line runs off the right edge, so the panes show a truncated line and
;; the horizontal scroll of one pane following the other.

;;; Code:

(require 'seq)
(require 'align-buffer)

(defun align-buffer-demo--bind-keys ()
  "Bind the demo's keys in this pane."
  (local-set-key (kbd "C-j") #'align-buffer-next-section)
  (local-set-key (kbd "C-k") #'align-buffer-previous-section)
  (local-set-key (kbd "RET") #'align-buffer-visit-source)
  (local-set-key (kbd "g") #'align-buffer-refresh)
  (local-set-key (kbd "q") #'align-buffer-quit))

(add-hook 'align-buffer-pane-mode-hook #'align-buffer-demo--bind-keys)

(defun align-buffer-demo--source (name lines mode)
  "Return a buffer called NAME holding LINES, under MODE."
  (let ((buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dolist (line lines) (insert line "\n")))
      (delay-mode-hooks (funcall mode))
      (font-lock-mode 1)
      (font-lock-ensure)
      (goto-char (point-min)))
    buffer))

(defun align-buffer-demo--lines (count prefix)
  "Return COUNT lines of C-like text labelled with PREFIX."
  (cl-loop for index from 1 to count
           collect (cond ((zerop (% index 20))
                          (format "\t// %s %d: a deliberately long line %s"
                                  prefix index
                                  (mapconcat #'identity
                                             (make-list 12 "carrying on") " ")))
                         ((zerop (% index 7))
                          (format "\tint %s_%d = %d;" prefix index index))
                         (t (format "\tcall_%s(%d);" prefix index)))))

;;;###autoload
(defun align-buffer-demo ()
  "Show a hand-written plan, to look at the core without a generator."
  (interactive)
  (let* ((shared (align-buffer-demo--lines 60 "shared"))
         (old-only (align-buffer-demo--lines 8 "removed"))
         (new-only (align-buffer-demo--lines 12 "added"))
         (old (align-buffer-demo--source
               "*align-buffer demo old*"
               (append (seq-take shared 20) old-only (seq-drop shared 20))
               #'c++-mode))
         (new (align-buffer-demo--source
               "*align-buffer demo new*"
               (append (seq-take shared 20) new-only (seq-drop shared 20))
               #'c++-mode))
         (rows nil)
         (old-line 1)
         (new-line 1))
    (dotimes (_ 20)
      (push (align-buffer-row-create
             :left (align-buffer-cell-create :kind 'line :source old :line old-line)
             :right (align-buffer-cell-create :kind 'line :source new :line new-line))
            rows)
      (cl-incf old-line)
      (cl-incf new-line))
    (dotimes (_ (length old-only))
      (push (align-buffer-row-create
             :left (align-buffer-cell-create :kind 'line :source old :line old-line
                                             :face 'diff-removed)
             :right (align-buffer-cell-create :kind 'blank :face 'diff-indicator-removed)
             :tag 'removed)
            rows)
      (cl-incf old-line))
    (dotimes (_ (length new-only))
      (push (align-buffer-row-create
             :left (align-buffer-cell-create :kind 'blank :face 'diff-indicator-added)
             :right (align-buffer-cell-create :kind 'line :source new :line new-line
                                              :face 'diff-added)
             :tag 'added)
            rows)
      (cl-incf new-line))
    (dotimes (_ 40)
      (push (align-buffer-row-create
             :left (align-buffer-cell-create :kind 'line :source old :line old-line)
             :right (align-buffer-cell-create :kind 'line :source new :line new-line))
            rows)
      (cl-incf old-line)
      (cl-incf new-line))
    (align-buffer-show
     (align-buffer-plan-create
      :rows (nreverse rows)
      :left-name "*align-buffer demo: old*"
      :right-name "*align-buffer demo: new*"
      :left-parameters (align-buffer-parameters-from-buffer old)
      :right-parameters (align-buffer-parameters-from-buffer new)))))

(provide 'align-buffer-demo)

;;; align-buffer-demo.el ends here
