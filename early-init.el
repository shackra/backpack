;; -*- no-byte-compile: t; -*-

(when (version< emacs-version "29.1")
  (error "Backpack is only compatible with Emacs version 29.1 and up"))

(setq package-enable-at-startup nil)
(setq load-prefer-newer t)
(setq native-comp-deferred-compilation nil)

;; directories
(defvar backpack--base-packages-dir (expand-file-name "base-packages" user-emacs-directory))
(defvar backpack--leaf (expand-file-name "leaf.el" backpack--base-packages-dir))
(defvar backpack--leaf-keywords (expand-file-name "leaf-keywords.el" backpack--base-packages-dir))
(defvar backpack--cache-emacs (expand-file-name ".cache/emacs" user-emacs-directory))

(let ((colors-file (expand-file-name "default-colors.el" backpack--cache-emacs)))
  (when (file-exists-p colors-file)
    (load colors-file)))

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
(setq auto-save-list-directory (expand-file-name "auto-save-list/" backpack--cache-emacs))

(unless (file-directory-p auto-save-list-directory)
  (make-directory auto-save-list-directory t))

(setq auto-save-list-file-prefix
      (expand-file-name ".saves-" auto-save-list-directory))

;; move eln-cache to another location
(setq backpack--eln-cache-dir (expand-file-name "eln-cache/" backpack--cache-emacs))

(unless (file-directory-p backpack--eln-cache-dir)
  (make-directory backpack--eln-cache-dir t))

(setq native-comp-eln-load-path (list backpack--eln-cache-dir))

;; configuration variables for elpaca
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name ".cache/elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues -100)
(elpaca `(,@elpaca-order))

(require 'leaf)
(require 'leaf-keywords)
(leaf-keywords-init)

;;; add additional keywords to leaf block
;; :doctor defines binaries to check on the user's system
;; :fonts check what if the fonts needed by a package are installed
(plist-put leaf-keywords :doctor '`(,@leaf--body))
(plist-put leaf-keywords :fonts '`(,@leaf--body))

;; alias :ensure to :elpaca
(setq leaf-alias-keyword-alist '((:ensure . :elpaca)))

(defun save-default-bg-fg-colors ()
  "Save the current theme's foreground and background colors into a file."
  (let ((bg (face-attribute 'default :background))
	(fg (face-attribute 'default :foreground)))
    (with-temp-file (expand-file-name "default-colors.el" backpack--cache-emacs)
      (insert (format "(setq default-frame-alist '((background-color . \"%s\") (foreground-color . \"%s\")))"
                      bg fg)))))

(leaf benchmark-init
  :ensure t
  :config
  (add-hook 'after-init-hook  #'benchmark-init/deactivate 100)
  (benchmark-init/activate))

(leaf no-littering
  :ensure t
  :pre-setq
  `(no-littering-etc-directory . ,(expand-file-name "etc" backpack--cache-emacs))
  `(no-littering-var-directory . ,(expand-file-name "var" backpack--cache-emacs))
  :config
  (with-eval-after-load "recentf"
    (add-to-list 'recentf-exclude
		 (recentf-expand-file-name no-littering-var-directory))
    (add-to-list 'recentf-exclude
		 (recentf-expand-file-name no-littering-etc-directory)))

  (no-littering-theme-backups)

  (defvar backpack-tree-sitter-installation-directory
    (no-littering-expand-var-file-name "tree-sitter")
    "Directory for installing tree-sitter grammars")

  (unless (file-exists-p backpack-tree-sitter-installation-directory)
    (make-directory backpack-tree-sitter-installation-directory t)))
