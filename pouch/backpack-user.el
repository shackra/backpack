(require 'xdg)
(require 'backpack-pouch)

(defvar backpack-user-directory ""
  "Location of the user configuration.")

(defvar backpack-user-directory-exists
  (or
   (when (file-exists-p (expand-file-name "backpack/init.el" (xdg-config-home)))
     (setq backpack-user-directory (expand-file-name "backpack/init.el" (xdg-config-home))))
   (when (file-exists-p "~/.backpack.d/init.el")
     (setq backpack-user-directory "~/.backpack.d/")))
  "Tell if there is a user configuration.")

(defun backpack-load-user-configuration ()
  "Load the user configuration.

This should set `backpack--gear'"
  (unless backpack-user-directory-exists
    (error "no user configuration exists, please create one"))
  (load (expand-file-name "init.el" backpack-user-directory) nil backpack-log-loading)
  (unless backpack--gear
    (warn "no configuration defined, was gear! even used?")))

(provide 'backpack-user)
