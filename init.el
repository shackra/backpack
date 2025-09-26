(require 'backpack-pouch)
(require 'backpack-manual)

(load-file (expand-file-name "gears/ui/theme.el" user-emacs-directory))

;; shamelessly copied from Emacs Bedrock (leave this at the end of the file, always)
(setq gc-cons-threshold (or backpack--initial-gc-threshold 800000))
