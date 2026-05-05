;; -*- lexical-binding: t; -*-
;;; backpack.el --- Backpack Emacs: A self-documenting GNU Emacs starter kit  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Jorge Javier Araya Navarro and Backpack contributors

;; Author: Jorge Javier Araya Navarro <jorge@esavara.cr>
;; URL: https://github.com/shackra/backpack

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Backpack is a self-documenting GNU Emacs starter kit inspired by
;; Bedrock and Doom Emacs.  Users declare features via the `gear!'
;; macro; Backpack loads only what is requested and auto-enables
;; sensible defaults that can be opted out of.

;;; Code:

(eval-and-compile
  (when (version< emacs-version "29.1")
    (error "Backpack is only compatible with Emacs version 29.1 and up")))

(let ((old-version (eval-when-compile emacs-version)))
  (unless (string= emacs-version old-version)
    (user-error (format "Backpack was compiled with Emacs %s, but was loaded with %s" emacs-version old-version))))

;; Copy the consistency imposed by Doom Emacs
(when (bound-and-true-p module-file-suffix)
  (push 'dynamic-modules features))
(when (fboundp #'json-parse-string)
  (push 'jansson features))
(when (string-match-p "HARFBUZZ" system-configuration-features)
  (push 'harfbuzz features))

;; don't bother with native compilation if it's not functional
(when (and (featurep 'native-compile)
	   (not (native-comp-available-p)))
  (delq 'native-compile features))

;; don't worry the user about obsolete macros
(put 'if-let 'byte-obsolete-info nil)
(put 'when-let 'byte-obsolete-info nil)

;; backpack standard library
(add-to-list 'load-path (expand-file-name "base-packages/leaf.el" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "base-packages/leaf-keywords.el" user-emacs-directory))
(add-to-list 'load-path (file-name-directory load-file-name))

(require 'leaf)
(require 'leaf-keywords)
(require 'backpack-platform)
(require 'backpack-pouch)
(require 'backpack-email-utils)
(require 'backpack-inventory)
(require 'backpack-treesit)

(define-key help-map (kbd "B") #'backpack-inventory)

(leaf-keywords-init)

;;; add additional keywords to leaf block
;; :doctor defines binaries to check on the user's system
;; :fonts check what if the fonts needed by a package are installed
(plist-put leaf-keywords :doctor '`(,@leaf--body))
(plist-put leaf-keywords :fonts '`(,@leaf--body))

;; alias :ensure to :elpaca
(setq leaf-alias-keyword-alist '((:ensure . :elpaca)))

;;; Backpack mode management
;; NOTE: This must be defined BEFORE the leaf advice below
(defvar backpack-mode 'normal
  "Current operating mode for Backpack.

Possible values:
- `normal': Standard Emacs startup - only activate pre-built packages
- `sync': Synchronization mode - install/build packages but skip activation")

(defun backpack-sync-mode-p ()
  "Return non-nil if Backpack is in synchronization mode."
  (eq backpack-mode 'sync))

(defun backpack-normal-mode-p ()
  "Return non-nil if Backpack is in normal mode."
  (eq backpack-mode 'normal))

;; NOTE: We previously tried to advise the `leaf` macro to filter keywords in sync mode,
;; but `leaf` is a macro, not a function, so `:around` advice doesn't work properly.
;; Instead, we rely on the elpaca advice (backpack--elpaca-skip-forms-in-sync-mode)
;; to prevent configuration forms from running during sync mode.

;;; Globals

(defvar backpack-init-time nil
  "The time it took to initialize Backpack.")

(defgroup backpack nil
  "A self-documenting GNU Emacs starter kit inspired after Bedrock and Doom."
  :link '(url-link "https://github.com/shackra/backpack"))

(defvar backpack-emacs-dir user-emacs-directory
  "The path of the currently loaded Emacs configuration.")

(defconst backpack-core-dir (file-name-directory load-file-name)
  "The directory of Backpack files.")

(defvar backpack-user-dir
  (let ((from-env (getenv-internal "BACKPACK_USER_DIR"))
	(in-xdg-config (expand-file-name "backpack/" (getenv-internal "XDG_CONFIG_HOME")))
	(in-user-home "~/.backpack.d/"))
    (cond
     (from-env from-env)
     ((file-exists-p in-user-home) in-user-home)
     (t in-xdg-config)))
  "Location of the user's private configuration.

Either ~/.config/backpack or ~/.backpack.d/.  Can be overridden
by the BACKPACK_USER_DIR environment variable.")

(defvar backpack-cache-dir (expand-file-name ".cache/" backpack-emacs-dir)
  "Location for local storage")

(defvar backpack-data-dir
  (expand-file-name "etc" backpack-cache-dir)
  "Location use by Backpack to store important files.

Delete this directory entails user intervention to make things work
again.")

(defvar backpack-nonessential-dir
  (expand-file-name "nonessentials" backpack-cache-dir)
  "Location where Backpack stores nonessential files.

If anything is missing here, Backpack Emacs will work as normal.")

(defvar backpack-state-dir
  (expand-file-name "state" backpack-cache-dir)
  "Location for files that carry state for some functionalities or packages.")

(defvar backpack-tree-sitter-installation-dir
  (expand-file-name "tree-sitter" backpack-nonessential-dir)
  "Location for treesit to install compiled grammar.")

(defvar backpack--treesit-state-file
  (expand-file-name "treesit-grammars.eld" backpack-state-dir)
  "File tracking compiled tree-sitter grammar revisions.
Each entry maps a language symbol to the git commit hash that was
used to compile its grammar.  This allows `backpack ensure' to
skip recompilation when the upstream grammar has not changed.")

;; These modules depend on directory variables defined above
(require 'backpack-defaults)
(require 'backpack-sync)
(require 'backpack-gc)

;;
;;; Startup optimizations
;;; copied straight from Doom Emacs 👀

(unless (daemonp)
  ;; PERF: `file-name-handler-alist' is consulted on each call to `require',
  ;;   `load', or various file/io functions (like `expand-file-name' or
  ;;   `file-remote-p'). You get a noteable boost to startup time by unsetting
  ;;   or simplifying its value.
  (let ((old-value (default-toplevel-value 'file-name-handler-alist)))
    (set-default-toplevel-value
     'file-name-handler-alist
     (if (eval-when-compile
           (locate-file-internal "calc-loaddefs.el" load-path))
         nil
       (list (rassq 'jka-compr-handler old-value))))
    (put 'file-name-handler-alist 'initial-value old-value)
    (define-advice command-line-1 (:around (fn args-left) respect-file-handlers)
      (let ((file-name-handler-alist (if args-left old-value file-name-handler-alist)))
        (funcall fn args-left)))
    (add-hook 'emacs-startup-hook
	      (defun backpack--reset-file-handler-alist-h ()
		(set-default-toplevel-value
		 'file-name-handler-alist
		 (delete-dups (append file-name-handler-alist old-value))))
	      100))

  (unless noninteractive
    (setq frame-inhibit-implied-resize t)

    (setq inhibit-startup-screen t
          inhibit-startup-echo-area-message user-login-name
          initial-major-mode 'fundamental-mode
          initial-scratch-message nil)

    (advice-add #'display-startup-echo-area-message :override #'ignore)
    (advice-add #'display-startup-screen :override #'ignore)

    (unless initial-window-system
      (define-advice tty-run-terminal-initialization (:override (&rest _) defer)
	(advice-remove #'tty-run-terminal-initialization #'tty-run-terminal-initialization@defer)
	(add-hook 'window-setup-hook
		  (apply-partially #'tty-run-terminal-initialization
				   (selected-frame) nil t))))

    (unless init-file-debug
      (put 'mode-line-format 'initial-value (default-toplevel-value 'mode-line-format))
      (setq-default mode-line-format nil)
      (dolist (buf (buffer-list))
        (with-current-buffer buf (setq mode-line-format nil)))
      (setq-default inhibit-redisplay t
                    inhibit-message t)
      (defun backpack--reset-inhibited-vars-h ()
        (setq-default inhibit-redisplay nil
                      inhibit-message nil)
        (remove-hook 'post-command-hook #'backpack--reset-inhibited-vars-h))
      (add-hook 'post-command-hook #'backpack--reset-inhibited-vars-h -100))

    (advice-add #'tool-bar-setup :override #'ignore)

    (put 'site-run-file 'initial-value site-run-file)
    (setq site-run-file nil)

    (define-advice startup--load-user-init-file (:around (fn &rest args) undo-hacks 95)
      "Undo Doom's startup optimizations to prep for the user's session."
      (unwind-protect (apply fn args)
	(setq-default inhibit-message nil)
	(advice-remove #'tool-bar-setup #'ignore)

	(add-hook 'tool-bar-mode-hook (defun --tool-bar-setup ()
					(tool-bar-setup)
					(remove-hook 'tool-bar-mode-hook '--tool-bar-setup)))
	(unless (default-toplevel-value 'mode-line-format)
	  (setq-default mode-line-format (get 'mode-line-format 'initial-value)))))

    (unless backpack--system-macos-p
      (setq command-line-ns-option-alist nil))
    (unless (memq initial-window-system '(x pgtk))
      (setq command-line-x-option-alist nil))))

;; Initialize elpaca
(backpack--ensure-elpaca)

(defvar backpack-after-init-hook nil
  "Abnormal hook for functions to be run after Backpack was initialized.
This hook fires before packages are activated.  Packages registered via
elpaca add their body forms during this phase.  Use
`backpack-user-after-init-hook' for setup code that depends on packages
being fully loaded.")

(defvar backpack-user-after-init-hook nil
  "Abnormal hook run after all packages have been activated and configured.
This hook fires after `backpack-after-init-hook' has completed (including
elpaca package activation and elpaca-after-init-hook).  Packages can
safely add functions to this hook during their elpaca body evaluation,
since the hook has not yet run at that point.  Use this hook for setup
code that requires packages to be fully loaded and configured.")

(defun backpack-start (&optional interactive?)
  "Start the Backpack session.
When INTERACTIVE? is non-nil, we're in a normal interactive Emacs session.
The behavior depends on `backpack-mode':
- In `normal' mode: only activate pre-built packages
- In `sync' mode: install/build packages without activation"
  (when (daemonp)
    (message "Starting in daemon mode...")
    (add-hook 'kill-emacs-hook
	      (lambda ()
		(message "Killing Emacs. ¡Adiós!"))
	      100))

  ;; Ensure required directories exist
  (with-file-modes 448
    (mapc (lambda (dir)
	    (make-directory dir t))
	  (list backpack-cache-dir
		backpack-nonessential-dir
		backpack-state-dir
		backpack-data-dir
		backpack-tree-sitter-installation-dir)))

  (if interactive?
      (progn
	;; Configure appropriate hook based on mode
	(cond
	 ((backpack-normal-mode-p)
	  ;; Normal mode: activate packages after init
	  (add-hook 'backpack-after-init-hook #'backpack--activate-packages))
	 ((backpack-sync-mode-p)
	  ;; Sync mode: build packages (without activation)
	  (add-hook 'backpack-after-init-hook #'backpack--sync-packages)))

	;; last hook to run in Emacs' startup process.
	(advice-add #'command-line-1 :after #'backpack-finalize)

	;; load user's private configuration
	(let ((init-file (expand-file-name "init.el" backpack-user-dir)))
	  (if (file-exists-p init-file)
              (pcase-let ((`(,gear-form . ,rest-forms)
                           (backpack--extract-gear-form init-file)))
                ;; Evaluate the gear! declaration first so backpack--gear
                ;; is populated before gears load
                (when gear-form
                  (eval gear-form))
                ;; Load gears -- their defaults are set based on backpack--gear
                (backpack-load-gear-files)
                ;; Now evaluate user customizations (without gear! forms)
                ;; so they override gear defaults
                (dolist (form rest-forms)
                  (eval form)))
            ;; No init file: load gears with empty configuration
            (backpack-load-gear-files)))
	  ;; load custom file
	  (load custom-file t))
    (progn ;; CLI/batch mode
      nil))

  ;; load site files
  (let ((site-loader
	 (lambda ()
	   (unless site-run-file
	     (when-let* ((site-file (get 'site-run-file 'initial-value)))
	       (let ((inhibit-startup-screen inhibit-startup-screen))
		 (setq site-run-file site-file)
		 (load site-file t)))))))

    (if interactive?
	(define-advice startup--load-user-init-file (:before (&rest _) load-site-files 100)
	  (funcall site-loader))
      (funcall site-loader)))
  t)

(defun backpack-finalize (&rest _)
  "After the startup process finalizes."
  (setq backpack-init-time (float-time (time-subtract (current-time) before-init-time)))

  ;; Run backpack hooks which will trigger package processing
  (run-hooks 'backpack-after-init-hook)

  ;; Run user-facing hook after packages are fully activated and configured.
  ;; Packages can add functions to this hook during their elpaca body
  ;; evaluation (which happens inside backpack-after-init-hook), and
  ;; those functions will run here, after all packages are ready.
  (run-hooks 'backpack-user-after-init-hook)

  (when (eq (default-value 'gc-cons-threshold) most-positive-fixnum)
    (setq-default gc-cons-threshold (* 16 1024 1024)))

  (when (backpack-normal-mode-p)
    (message "Backpack initialized in %.2fs" backpack-init-time))
  t)

(defun backpack-load-gear-files ()
  "Load all gears available."
  (load (expand-file-name "gears/config/default" backpack-core-dir))
  (load (expand-file-name "gears/ui/theme" backpack-core-dir))
  (load (expand-file-name "gears/ui/treesit" backpack-core-dir))
  (load (expand-file-name "gears/completion/corfu" backpack-core-dir))
  (load (expand-file-name "gears/completion/eglot" backpack-core-dir))
  (load (expand-file-name "gears/completion/marginalia" backpack-core-dir))
  (load (expand-file-name "gears/completion/nerd-icons-completion" backpack-core-dir))
  (load (expand-file-name "gears/completion/orderless" backpack-core-dir))
  (load (expand-file-name "gears/completion/vertico" backpack-core-dir))
  (load (expand-file-name "gears/tools/eldoc" backpack-core-dir))
  (load (expand-file-name "gears/tools/envrc" backpack-core-dir))
  (load (expand-file-name "gears/tools/magit" backpack-core-dir))
  (load (expand-file-name "gears/tools/cool-motions" backpack-core-dir))
  (load (expand-file-name "gears/tools/dired" backpack-core-dir))
  (load (expand-file-name "gears/tools/whitespaces" backpack-core-dir))
  (load (expand-file-name "gears/checkers/spellchecking" backpack-core-dir))
  (load (expand-file-name "gears/email/mu4e" backpack-core-dir))
  (load (expand-file-name "gears/term/vterm" backpack-core-dir))
  (load (expand-file-name "gears/term/eshell" backpack-core-dir))
  (load (expand-file-name "gears/ai/anvil" backpack-core-dir))
  (load (expand-file-name "gears/os/windows" backpack-core-dir))
  (load (expand-file-name "gears/editing/c" backpack-core-dir))
  (load (expand-file-name "gears/editing/cmake" backpack-core-dir))
  (load (expand-file-name "gears/editing/cpp" backpack-core-dir))
  (load (expand-file-name "gears/editing/emacs-lisp" backpack-core-dir))
  (load (expand-file-name "gears/editing/go" backpack-core-dir))
  (load (expand-file-name "gears/editing/haskell" backpack-core-dir))
  (load (expand-file-name "gears/editing/hyprland" backpack-core-dir))
  (load (expand-file-name "gears/editing/json" backpack-core-dir))
  (load (expand-file-name "gears/editing/latex" backpack-core-dir))
  (load (expand-file-name "gears/editing/lua" backpack-core-dir))
  (load (expand-file-name "gears/editing/make" backpack-core-dir))
  (load (expand-file-name "gears/editing/markdown" backpack-core-dir))
  (load (expand-file-name "gears/editing/nix" backpack-core-dir))
  (load (expand-file-name "gears/editing/objc" backpack-core-dir))
  (load (expand-file-name "gears/editing/org" backpack-core-dir))
  (load (expand-file-name "gears/editing/python" backpack-core-dir))
  (load (expand-file-name "gears/editing/rst" backpack-core-dir))
  (load (expand-file-name "gears/editing/rust" backpack-core-dir))
  (load (expand-file-name "gears/editing/terraform" backpack-core-dir))
  (load (expand-file-name "gears/editing/toml" backpack-core-dir))
  (load (expand-file-name "gears/editing/yaml" backpack-core-dir)))

(provide 'backpack)
;;; backpack.el ends here
