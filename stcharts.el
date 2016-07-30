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
       ((string= (plist-get (gethash idx charts) 'title) title) idx)
       (t (st--get-index-by-title title (incf idx) st-charts)))
    (st--get-index-by-title title 0 st-charts)))

(defun st--child-chart-pp (idx)
  (let ((chart (gethash idx st-charts))
        (complete (if (plist-get chart 'complete) "X" " ")))
    (concat "- " complete (plist-get chart 'title) "\n\n")))

(defun st--insert-chart ()
  (insert "\nIdeal:\n")
  (setq ideal-start (point-marker))
  (insert (plist-get chart 'ideal))
  (setq ideal-end (point-marker))

  (insert "\n\n")
  (st--insert-children)

  (insert "\nReal:\n")
  (setq real-start (point-marker))
  (insert (plist-get chart 'real))
  (setq real-end (point-marker))

  (when (plist-get chart 'parents)
    (insert "\n\nRelated charts:\n")
    (st--insert-parents)))

(defun st--insert-children ()
  (let ((children (mapcar (lambda (i) (cons i (gethash i st-charts)))
                          (plist-get chart 'children))))
    (mapcar
     (lambda (child)
       (insert "- [" (if (plist-get (cdr child) 'complete) "X" " ") "] ")
       (setq child-button-start (point))
       (insert (plist-get (cdr child) 'title))
       (make-button child-button-start (point)
                    'action `(lambda (x) (st--by-index ,(car child))))
       (insert "\n"))
     children)))

(defun st--insert-parents ()
  (let ((parents (mapcar (lambda (i) (cons i (gethash i st-charts)))
                         (plist-get chart 'parents))))
    (mapcar
     (lambda (parent)
       (setq parent-button-start (point))
       (insert (plist-get (cdr parent) 'title))
       (make-button parent-button-start (point)
                    'action `(lambda (x) (st--by-index ,(car parent))))
       (insert ", "))
     parents)
    (backward-delete-char 2)))

(defun st-save-chart ()
  (puthash index chart st-charts))

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
  (let* ((count (hash-table-count st-charts))
         (titles (let (titles)
                   (reverse
                    (dotimes (i count titles)
                      (setq titles (cons (plist-get (gethash i st-charts) 'title) titles)))))))
    (setq title (completing-read "Chart Title: " titles))
    (setq idx (st--get-index-by-title title))
    (when (= idx -1)
      (setq idx (if (string= "" title) 0 (st--create-new-chart title))))
    (st--by-index idx)))

(defun st--by-index (idx)
  (kill-local-variable 'chart)
  (setq chart (gethash idx st-charts))
  (switch-to-buffer (st--generate-buffer-name (plist-get chart 'title)))
  (kill-all-local-variables)
  (make-local-variable 'index)
  (make-local-variable 'chart)
  (setq major-mode 'st-chart-mode mode-name "ST Charts")
  (use-local-map st-chart-mode-map)
  (st--insert-chart))

