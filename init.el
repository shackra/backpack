(require 'backpack-pouch)
(require 'backpack-user)

(backpack-load-user-configuration)

(load (expand-file-name "gears/config/default.el" user-emacs-directory))
(load (expand-file-name "gears/ui/theme.el" user-emacs-directory))
(load (expand-file-name "gears/tools/magit.el" user-emacs-directory))
(load (expand-file-name "gears/checkers/spellchecking.el" user-emacs-directory))
(load (expand-file-name "gears/programming/emacs-lisp.el" user-emacs-directory))
(load (expand-file-name "gears/programming/go.el" user-emacs-directory))

;; shamelessly copied from Emacs Bedrock (leave this at the end of the file, always)
(setq gc-cons-threshold (or backpack--initial-gc-threshold 800000))
