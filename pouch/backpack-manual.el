(defun load-gear (dir)
  "Load init.el file in DIR."
  (let ((path (expand-file-name (format "gears/%s" dir))))
    (load (expand-file-name "init.el" path) nil t)))

(provide 'backpack-manual)
