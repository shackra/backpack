(when (version< emacs-version "27.1")
  (error "Backpack is only compatible with Emacs version 27.1 and up"))

(setq package-enable-at-startup nil)
(setq load-prefer-newer t)

;; directories
(defvar backpack--base-packages-dir (expand-file-name "base-packages" user-emacs-directory))
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
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(require 'leaf)
(require 'leaf-keywords)
(leaf-keywords-init)

;; alias :ensure to :elpaca
(setq leaf-alias-keyword-alist '((:ensure . :elpaca)))
