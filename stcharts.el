;;; stcharts.el --- Emacs plugin for Robert Fritz style planning charts

;; Copyright (C) 2016-2017 Richard Kallos

(require 'cl-lib)
(defcustom st-charts-file
  "~/.emacs.d/site-lisp/stcharts/charts.el"
  "This variable contains the filename where your ST charts are stored."
  :type 'sexp)

(setq st-chart-mode-map
      (let ((m (make-sparse-keymap)))
        (suppress-keymap m)))

(defun st--load-charts-file ()
  (setq buf (find-file-read-only st-charts-file))
  (set-buffer buf)
  (setq m (point-min-marker))
  (setq st-charts (read m))
  (kill-buffer buf))

(defun st--save-charts-file ()
  (with-temp-file st-charts-file
    (print st-charts (current-buffer))))

(defun st--save-chart ()
  (st--put chart 'title (buffer-substring title-start title-end))
  (st--put chart 'ideal (buffer-substring ideal-start ideal-end))
  (st--put chart 'real  (buffer-substring real-start real-end)))

(defun st--generate-buffer-name (title)
  (format "*ST* %s" title))

(defun st--create-new-chart (title)
  (let ((idx (hash-table-count st-charts)))
    (puthash idx (st--empty-chart) st-charts)
    idx))

(defun st--empty-chart ()
  (list title ""
        ideal "Ideal"
        real "Real"
        due ""
        complete nil
        children nil
        parents nil))

(defmacro st--get (idx-or-plist &optional field)
  "Macro for structure-agnostic retrieving of a field from a chart"
  (let ((chart (if (listp idx-or-plist) idx-or-plist `(gethash ,idx-or-plist st-charts))))
    (if field
        `(plist-get chart ,field)
      chart)))

(defmacro st--put (idx-or-plist field val)
  "Macro for structure-agnostic setting of a field in a chart"
  (let ((chart (if (listp idx-or-plist) idx-or-plist `(gethash ,idx-or-plist st-charts))))
    `(plist-put ,chart ,field ,val)))

(defun st--get-index-by-title (title)
  "Does a lookup of st-charts, returning the index of the matching chart,
or -1 if no such chart is found"
  (cl-do* ((idx 0 (+ 1 idx))
           (current-title (st--get idx 'title) (st--get idx 'title)))
      ((or (string= current-title title)
           (null current-title))
       (if (null current-title) -1 idx))))

(defun st--child-chart-pp (idx)
  (let ((chart (st--get idx))
        (complete (if (st--get chart 'complete) "X" " ")))
    (concat "- " complete (st--get chart 'title) "\n\n")))

(defun st--insert-chart ()
  (setq title-start (point-marker))
  (make-local-variable 'title-start)
  (insert (st--get chart 'title) "\n")
  (setq title-end (point-marker))
  (make-local-variable 'title-end)

  (insert "Ideal:\n")
  (setq ideal-start (point-marker))
  (make-local-variable 'ideal-start)
  (insert (st--get chart 'ideal) "\n")
  (setq ideal-end (point-marker))
  (make-local-variable 'ideal-end)

  (insert "\n")
  (st--insert-children)

  (insert "\nReal:\n")
  (setq real-start (point-marker))
  (make-local-variable 'real-start)
  (insert (st--get chart 'real) "\n")
  (setq real-end (point-marker))
  (make-local-variable 'real-end)

  (when (st--get chart 'parents)
    (insert "\n\nRelated charts:\n")
    (st--insert-parents)))

(defun st--insert-children ()
  (let ((children (mapcar (lambda (i) (cons i (gethash i st-charts)))
                          (st--get chart 'children))))
    (mapcar
     (lambda (child)
       (insert "- [" (if (st--get (cdr child) 'complete) "X" " ") "] ")
       (setq child-button-start (point))
       (insert (st--get (cdr child) 'title))
       (make-button child-button-start (point)
                    'action `(lambda (x) (st--by-index ,(car child))))
       (insert "\n"))
     children)))

(defun st--insert-parents ()
  (let ((parents (mapcar (lambda (i) (cons i (gethash i st-charts)))
                         (st--get chart 'parents))))
    (mapcar
     (lambda (parent)
       (setq parent-button-start (point))
       (insert (st--get (cdr parent) 'title))
       (make-button parent-button-start (point)
                    'action `(lambda (x) (st--by-index ,(car parent))))
       (insert ", "))
     parents)
    ;; Remove trailing comma
    (backward-delete-char 2)))

(defun st-save-chart ()
  (puthash index chart st-charts))

(defun st-insert-link-to-chart ()
  "Choose a chart to link to in the current chart"
  (interactive)
  (setq idx -1)
  (while (= idx -1)
    (setq title (st--prompt-for-title "Insert Link: "))
    (setq idx (st--get-index-by-title title))
    (when (= idx -1)
      (setq yes-create (yes-or-no-p (format "Insert new chart: \"%s\"?" title)))
      (when yes-create
        (setq idx (if (= )) (st--create-new-chart title)))))
  ;; TODO: Add link to current buffer
  )
(defun st-remove-chart-at-point ())
(defun st-move-child-at-point (pos))

(defun st-add-parent (parent))
(defun st-remove-parent (parent))
(defun st-add-child (child))
(defun st-remove-child (child))

(defun st ()
  "Choose a chart to open, or create a new chart."
  (interactive)
  (setq title (st--prompt-for-title "Chart Title: "))
  (setq idx (st--get-index-by-title title))
  (when (= idx -1)
    (setq idx (if (string= "" title) 0 (st--create-new-chart title))))
  (st--by-index idx))

(defun st--prompt-for-title (prompt)
  (setq titles ())
  (maphash (lambda (key value)
             (setq titles (cons (st--get value 'title) titles)))
           st-charts)
  (setq titles (nreverse titles))
  (completing-read prompt titles))

(defun st--by-index (index)
  (kill-local-variable 'chart)
  (setq chart (gethash index st-charts))
  (switch-to-buffer (get-buffer-create
                     (st--generate-buffer-name (st--get chart 'title))))
  (kill-all-local-variables)
  (make-local-variable 'index)
  (make-local-variable 'chart)
  (setq major-mode 'st-chart-mode mode-name "ST Charts")
  (use-local-map st-chart-mode-map)
  (erase-buffer)
  (st--insert-chart))

(defun st--delete-chart (idx)
  (remhash idx st-charts))
