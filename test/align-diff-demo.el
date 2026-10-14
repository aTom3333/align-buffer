;;; align-diff-demo.el --- A diff to look at -*- lexical-binding: t; -*-

;;; Commentary:

;; `M-x align-diff-demo' shows two real buffers side by side with a real diff
;; between them, so the colours and the alignment can be watched together.
;;
;; The file is one plausible source file against the revision before it, and
;; the text says which case each part of it is there for: a line changed in
;; place, a line split in two, a run only one side has, a hunk whose two sides
;; are of different lengths, a line whose two versions wrap differently, and a
;; block moved elsewhere, which a diff reports as a deletion and an addition
;; far apart.  Between them are stretches that are the same on both sides, long
;; enough to scroll through.
;;
;; Both buffers are editable, and that is worth trying: type in either one and
;; after `align-diff-idle-delay' the diff is run again, the colours are painted
;; afresh, and the windows come level again if what corresponds has changed.

;;; Code:

(require 'align-diff)

(defun align-diff-demo--block (name count)
  "Return a block of COUNT lines called NAME, as a list of strings.
Each line carries its number within the block, so where you are inside one
taller than the window can be read off the screen."
  (append
   (list (format "void %s(const Samples& samples) {" name))
   (mapcar (lambda (index)
             (format "  Check(samples[%d], \"%s line %d of %d\");"
                     index name index (- count 3)))
           (number-sequence 1 (- count 3)))
   (list "}" "")))

(defconst align-diff-demo--entries
  `((same "// Sums a batch of samples, weighted."
          "//"
          "// A review of this file against the revision before it.  Lines that"
          "// differ carry the diff colour of their side, and the characters"
          "// that differ inside a hunk carry a second colour on top of it."
          ""
          "#include \"weights.h\""
          "#include \"samples.h\""
          ""
          "namespace batch {"
          ""
          "// The two sides start out the same, for long enough to scroll."
          "double Sum(const Samples& samples) {"
          "  double total = 0;")
    (filler 14)
    (same ""
          "  // Changed in place.  One word differs, and that word alone is"
          "  // marked inside the line.")
    (changed ("  return total / samples.Count();")
             ("  return total / samples.Size();"))
    (same "}"
          ""
          "// Nothing happens here for a while."
          "void Reset(Weights& weights) {")
    (filler 10)
    (same "}"
          ""
          "// One line split into two.  The hunk is refined whole, so the"
          "// argument moved to the second line is not called removed on one"
          "// side and added on the other.")
    (changed ("  Fit(samples, weights, tolerance);")
             ("  Fit(samples, weights,"
              "      tolerance_in_metres);"))
    (same "}"
          "")
    (filler 8)
    (same ""
          "// A run only the new side has, paired with nothing, so the two"
          "// windows jump across it rather than drifting apart.")
    (added "double Median(const Samples& samples) {"
           "  auto sorted = samples.Sorted();"
           "  const auto middle = sorted.Size() / 2;"
           "  if (sorted.Size() % 2 == 0)"
           "    return (sorted[middle - 1] + sorted[middle]) / 2;"
           "  return sorted[middle];"
           "}"
           "")
    (filler 6)
    (same ""
          "// And a run only the old side has, which is the same thing the other"
          "// way round.")
    (removed "double Mode(const Samples& samples) {"
             "  Histogram counts(samples);"
             "  return counts.MostCommon();"
             "}"
             "")
    (filler 6)
    (same ""
          "// A big run the new side alone has, better than half a screenful of"
          "// it, so the old pane stands a block of blank rows against it.")
    (added "class WeightedWindow {"
           " public:"
           "  WeightedWindow(const Weights& weights, int span)"
           "      : weights_(weights), span_(span) {"
           "    if (span_ <= 0)"
           "      throw std::invalid_argument(\"span must be positive\");"
           "  }"
           ""
           "  double Mean(const Samples& samples) const {"
           "    double total = 0;"
           "    double weight = 0;"
           "    for (int index = 0; index < span_; ++index) {"
           "      total += weights_[index] * samples[index];"
           "      weight += weights_[index];"
           "    }"
           "    return weight > 0 ? total / weight : 0;"
           "  }"
           ""
           "  double Variance(const Samples& samples) const {"
           "    const double mean = Mean(samples);"
           "    double total = 0;"
           "    for (int index = 0; index < span_; ++index)"
           "      total += Square(samples[index] - mean);"
           "    return total / span_;"
           "  }"
           ""
           " private:"
           "  Weights weights_;"
           "  int span_ = 0;"
           "};"
           "")
    (filler 6)
    (same ""
          "// And a big run the old side alone has, which is the same block of"
          "// blank rows in the other pane.")
    (removed "double RollingMean(const Samples& samples, int span) {"
             "  if (span <= 0)"
             "    return 0;"
             "  double total = 0;"
             "  for (int index = 0; index < span; ++index)"
             "    total += samples[index];"
             "  return total / span;"
             "}"
             ""
             "double RollingVariance(const Samples& samples, int span) {"
             "  const double mean = RollingMean(samples, span);"
             "  double total = 0;"
             "  for (int index = 0; index < span; ++index)"
             "    total += Square(samples[index] - mean);"
             "  return total / span;"
             "}"
             ""
             "void PrintRolling(const Samples& samples, int span) {"
             "  Print(\"mean\", RollingMean(samples, span));"
             "  Print(\"variance\", RollingVariance(samples, span));"
             "  Print(\"span\", span);"
             "}"
             "")
    (filler 6)
    (same ""
          "// A hunk with four lines on the old side and two on the new.  The"
          "// first two pair up, and the last two of the old side stand alone.")
    (changed ("  const double scale = Scale(weights);"
              "  const double offset = Offset(weights);"
              "  total *= scale;"
              "  total += offset;")
             ("  const Transform transform = Transform::Of(weights);"
              "  total = transform.Apply(total);"))
    (filler 6)
    (same ""
          "// And a hunk the other way about: two lines on the old side against"
          "// five on the new.")
    (changed ("  if (samples.Empty())"
              "    return 0;")
             ("  if (samples.Empty()) {"
              "    Log(\"nothing to sum\");"
              "    return 0.0;"
              "  }"
              "  Log(\"summing %d samples\", samples.Count());"))
    (filler 8)
    (same ""
          "// A changed line whose two versions are of very different lengths,"
          "// so with the buffers wrapped the pair takes one screen row on the"
          "// old side and three on the new.  That is the case a row cannot"
          "// stand for on both sides at once.")
    (changed ("  Report(total);")
             (,(concat "  Report(total, \"the weighted sum of every sample in "
                       "the batch, taken after the transform has been applied "
                       "and the empty case has been dealt with\");")))
    (filler 12)
    (same ""
          "// A block moved further down.  A diff has no word for a move, so"
          "// this is where the old side has it:")
    (removed "void Describe(const Samples& samples) {"
             "  Print(\"count\", samples.Count());"
             "  Print(\"first\", samples.First());"
             "  Print(\"last\", samples.Last());"
             "}"
             "")
    (filler 8)
    (same ""
          "// and this is where the new side has it, word for word the same.")
    (added "void Describe(const Samples& samples) {"
           "  Print(\"count\", samples.Count());"
           "  Print(\"first\", samples.First());"
           "  Print(\"last\", samples.Last());"
           "}"
           "")
    (filler 8)
    (same ""
          "// A run the new side alone has that is taller than a window, so the"
          "// old pane stands a whole viewport of blank rows against it and no"
          "// position in the buffer falls anywhere inside them.")
    (added ,@(align-diff-demo--block "Checked" 80))
    (filler 8)
    (same ""
          "// And one the old side alone has, taller than a window as well.")
    (removed ,@(align-diff-demo--block "Legacy" 80))
    (filler 8)
    (same ""
          "// The same on both sides to the end, the way a review usually"
          "// finishes."
          "}  // namespace batch"))
  "The file to build, as entries saying which side has what.
`same' and `filler' lines are on both sides, `removed' lines on the old side
alone, `added' lines on the new side alone, and a `changed' entry carries the
old side's lines and the new side's.")

(defun align-diff-demo--filler (number)
  "Return the NUMBER-th line of the stretches both sides share."
  (format "  total += weights[%d] * samples[%d];" number number))

(defun align-diff-demo--build ()
  "Return the two sides' text, as (OLD . NEW)."
  (let ((old nil)
        (new nil)
        (filled 0))
    (dolist (entry align-diff-demo--entries)
      (pcase entry
        (`(filler ,count)
         (dotimes (_ count)
           (let ((line (align-diff-demo--filler filled)))
             (setq filled (1+ filled))
             (push line old)
             (push line new))))
        (`(changed ,old-lines ,new-lines)
         (dolist (line old-lines) (push line old))
         (dolist (line new-lines) (push line new)))
        (`(same . ,lines)
         (dolist (line lines) (push line old) (push line new)))
        (`(removed . ,lines) (dolist (line lines) (push line old)))
        (`(added . ,lines) (dolist (line lines) (push line new)))))
    (cons (concat (mapconcat #'identity (nreverse old) "\n") "\n")
          (concat (mapconcat #'identity (nreverse new) "\n") "\n"))))

(defun align-diff-demo--buffer (name text)
  "Return a buffer called NAME holding TEXT, under a mode worth reading it in."
  (let ((buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (remove-overlays)
        (insert text))
      (delay-mode-hooks (prog-mode))
      (font-lock-mode 1)
      ;; The line numbers, so which lines are level can be read off the screen.
      (display-line-numbers-mode 1)
      ;; Wrapped, so the changed line of very different lengths takes more rows
      ;; on one side than on the other.
      (visual-line-mode 1)
      (setq-local scroll-margin 5)
      ;; A row at a time where a scroll is needed at all, as a reader's own
      ;; configuration has it, rather than the recentring Emacs does by default.
      (setq-local scroll-conservatively 101)
      ;; A screen line at a time, which moves the window without moving point
      ;; and so drives the sync from the other end than the arrow keys do.
      (local-set-key (kbd "M-<down>") #'scroll-up-line)
      (local-set-key (kbd "M-<up>") #'scroll-down-line)
      (local-set-key (kbd "C-c C-r") #'align-diff-refresh)
      (set-buffer-modified-p nil)
      (goto-char (point-min)))
    buffer))

;;;###autoload
(defun align-diff-demo ()
  "Show one file against the revision before it, diffed and aligned."
  (interactive)
  (dolist (name '("*align-diff demo old*" "*align-diff demo new*"))
    (when-let ((buffer (get-buffer name)))
      (when-let ((session (align-buffer-session-for buffer)))
        (align-buffer-quit session))))
  (pcase-let* ((`(,old-text . ,new-text) (align-diff-demo--build))
               (session (align-diff-buffers
                         (align-diff-demo--buffer "*align-diff demo old*"
                                                  old-text)
                         (align-diff-demo--buffer "*align-diff demo new*"
                                                  new-text))))
    (message (concat "The text says what each part is there for.  Editing "
                     "either side re-diffs it; C-c C-r does it now; "
                     "M-x align-buffer-quit ends it."))
    session))

(provide 'align-diff-demo)

;;; align-diff-demo.el ends here
