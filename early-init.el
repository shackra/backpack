(setq package-enable-at-startup nil)

(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name ".cache/elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))

(add-to-list 'load-path (expand-file-name "base-packages/elpaca" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "base-packages/leaf.el" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "base-packages/leaf-keywords.el" user-emacs-directory))

(require 'elpaca)
(require 'leaf)
(require 'leaf-keywords)

(leaf-keywords-init)

(setq leaf-alias-keyword-alist '((:ensure . :elpaca)))
(add-hook 'after-init-hook #'elpaca-process-queues)
