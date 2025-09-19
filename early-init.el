(setq package-enable-at-startup nil)

;; shamelessly copied from Emacs Bedrock
(setq backpack--initial-gc-threshold gc-cons-threshold)
(setq gc-cons-threshold 10000000)

(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)

(setq inhibit-startup-echo-area-message (user-login-name))

;; move auto-save-list to another location
(setq auto-save-list-directory (expand-file-name ".cache/emacs/auto-save-list/" user-emacs-directory))

(unless (file-directory-p auto-save-list-directory)
  (make-directory auto-save-list-directory t))

(setq auto-save-list-file-prefix
      (expand-file-name ".saves-" auto-save-list-directory))

;; move eln-cache to another location
(setq backpack--eln-cache-dir (expand-file-name ".cache/emacs/eln-cache/" user-emacs-directory))

(unless (file-directory-p backpack--eln-cache-dir)
  (make-directory backpack--eln-cache-dir t))

(setq native-comp-eln-load-path (list backpack--eln-cache-dir))

;; configuration variables for elpaca
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

;; alias :ensure to :elpaca
(setq leaf-alias-keyword-alist '((:ensure . :elpaca)))

(add-hook 'after-init-hook #'elpaca-process-queues)
