(require 'ert)

(defvar backpack--base-packages-dir (expand-file-name "base-packages" user-emacs-directory))
(defvar backpack--leaf (expand-file-name "leaf.el" backpack--base-packages-dir))
(defvar backpack--leaf-keywords (expand-file-name "leaf-keywords.el" backpack--base-packages-dir))

(add-to-list 'load-path backpack--leaf)
(add-to-list 'load-path backpack--leaf-keywords)
(add-to-list 'load-path (expand-file-name "pouch" user-emacs-directory))

(load-file (expand-file-name  "test/pouch/backpack-pouch.el" user-emacs-directory))

(provide 'all-tests)
