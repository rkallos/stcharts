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

(defun st--chart-pp (idx)
  (let ((chart (nth st-charts idx)))
    (insert "Ideal:\n"
            (plist-get chart 'ideal)
            "\n\n"
            (mapcar
             (lambda (i)
               (concat (plist-get (nth st-charts i) 'title)
                       "\n"))
             (plist-get chart 'children))
            "\nReal:\n"
            (plist-get chart 'real)
            )))

(defun st-add-parent (parent))
(defun st-remove-parent (parent))
(defun st-add-child (child))
(defun st-remove-child (child))

(defun st (title)
  "Choose a chart to open, or create a new chart."
  (interactive "sChart Title: ")
  (setq idx (st--get-index-by-title title))
  (unless (= idx -1)
    (setq idx (if (string= "" title) 0 (st--create-new-chart title))))
  (switch-to-buffer (st--generate-buffer-name title))
  (kill-all-local-variables)
  (setq major-mode 'st-chart-mode
        mode-name "ST Charts")
  (use-local-map st-chart-mode-map)
  (let ((chart (nth idx st-charts))
        (ewoc (ewoc-create 'st--chart-pp
                           (format "ST Charts: %s" (plist-get chart 'title))
                           (substitute-command-keys
                            "\n\\{st-chart-mode-map}"))))
    (ewoc-enter-last ewoc idx)
    (ewoc-enter-last ewoc nil)))
