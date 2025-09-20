(defvar backpack--gear '()
  "List of gear in use by the user.")

(defvar backpack-log-loading nil
  "Tell Backpack to log every file load.")

(defmacro gear! (&rest gear)
  "Declare the GEAR to use."
  `(setq backpack--gear ',gear))

(defun backpack--gear-load-gear (gears)
  "Load GEARS defined by `gear!`."
  (while gears
    (let* ((category (pop gears))
           (mods     (pop gears)))
      (dolist (m mods)
        (let* ((name (if (symbolp m) m (car m)))
               (flags (when (listp m) (cdr m)))
               (path (expand-file-name
                      (format "gear/%s/%s" category name)
                      user-emacs-directory)))
	  (when backpack-log-loading
            (message "Loading gear %s with flags %s" name flags))
          (when (file-directory-p path)
            (let ((init-file (expand-file-name "init.el" path)))
              (when (file-exists-p init-file)
                (load init-file nil t)))))))))

(provide 'pouch)
