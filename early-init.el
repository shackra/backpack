(when (version< emacs-version "27.1")
  (error "Backpack is only compatible with Emacs version 27.1 and up"))

(setq package-enable-at-startup nil)
(setq load-prefer-newer t)

;; directories
(defvar backpack--base-packages-dir (expand-file-name "base-packages" user-emacs-directory))
(defvar backpack--elpaca (expand-file-name "elpaca" backpack--base-packages-dir))
(defvar backpack--leaf (expand-file-name "leaf.el" backpack--base-packages-dir))
(defvar backpack--leaf-keywords (expand-file-name "leaf-keywords.el" backpack--base-packages-dir))

(add-to-list 'load-path backpack--leaf)
(add-to-list 'load-path backpack--leaf-keywords)
(add-to-list 'load-path (expand-file-name "pouch" user-emacs-directory))

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

;; this is a slimmed down version of the bootstrap code offered in the readme
;; due to shipping elpaca already as a Git submodule
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (default-directory repo)) ;; everything happens in the repo directory
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo) 		;; copy the Git submodule to ~/.emacs.d/.cache/elpaca/repo/elpaca
    (copy-directory backpack--elpaca repo t t t))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))

(add-hook 'after-init-hook #'elpaca-process-queues)

(require 'leaf)
(require 'leaf-keywords)
(leaf-keywords-init)

;; alias :ensure to :elpaca
(setq leaf-alias-keyword-alist '((:ensure . :elpaca)))
