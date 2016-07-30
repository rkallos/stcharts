(defcustom st-charts
  "((title \"Root Chart\" ideal \"\" real \"\" due \"\" completed nil children '() parents '()))"
  "This variable contains the combined graph of all your ST charts.
This should only be edited via st-charts buffers, not manually"
  :type 'sexp)

(setq st-chart-mode-map
      (let ((m (make-sparse-keymap)))
        (suppress-keymap m)))

(defun st--generate-buffer-name (title)
  (format "*ST* %s" title))

(defun st--create-new-chart (title) 0)

(defun st--get-index-by-title (title &optional idx charts)
  "Does a lookup of st-charts, returning the index of the matching chart,
or -1 if no such chart is found"
  (if idx
      (cond
       ((not charts) -1)
       ((string= (plist-get (car charts) 'title) title) idx)
       (t (st--get-index-by-title title (+ idx 1) (cdr charts))))
    (st--get-index-by-title title 0 st-charts)))

(defun st--child-chart-pp (idx)
  (let* ((chart (nth idx st-charts))
         (complete (if (plist-get chart 'complete) "X" " ")))
    (concat "- " complete (plist-get chart 'title) "\n\n")))

(defun st--render-chart ()
  (insert "\nIdeal:\n"
          (plist-get chart 'ideal)
          "\n\n"
          (apply
           'concat
           (mapcar
            (lambda (i) (st--child-chart-pp i))
            (plist-get chart 'children)))
          "Real:\n"
          (plist-get chart 'real)

          (when (plist-get chart 'parents)
            "\n\nRelated charts:\n"
            (substring
             (apply
              'concat
              (mapcar
               (lambda (i)
                 (concat (plist-get (nth i st-charts) 'title) ", "))
               (plist-get chart 'parents)))
             0 -2))))

(defun st-insert-chart-at-point ())
(defun st-remove-chart-at-point ())
(defun st-move-child-at-point (pos))

(defun st-add-parent (parent))
(defun st-remove-parent (parent))
(defun st-add-child (child))
(defun st-remove-child (child))

(defun st ()
  "Choose a chart to open, or create a new chart."
  (interactive)
  (let ((titles (mapcar* #'cons
                         (mapcar (lambda (l) (plist-get l 'title)) st-charts)
                         (number-sequence 0 (length st-charts)))))
    (setq title (completing-read "Chart Title: " titles))
    (setq idx (st--get-index-by-title title))
    (when (= idx -1)
      (setq idx (if (string= "" title) 0 (st--create-new-chart title))))
    (st--by-index idx)))

(defun st--by-index (index)
  (setq chart (nth index st-charts))
  (switch-to-buffer (st--generate-buffer-name (plist-get chart 'title)))
  (kill-all-local-variables)
  (make-local-variable 'index)
  (make-local-variable 'chart)
  (setq major-mode 'st-chart-mode mode-name "ST Charts")
  (use-local-map st-chart-mode-map)
  (st--render-chart))

