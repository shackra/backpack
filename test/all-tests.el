(require 'ert)

(load (expand-file-name "early-init.el" user-emacs-directory))
(load (expand-file-name "init.el" user-emacs-directory))

(load-file (expand-file-name  "test/startup-time.el" user-emacs-directory))

(provide 'all-tests)
