;;; ensure.el --- Backpack synchronization mode -*- lexical-binding: t; -*-
;;
;; This file implements the `backpack ensure' command which:
;; 1. Installs elpaca from base-packages (no internet required for elpaca itself)
;; 2. Installs all missing packages needed by enabled gears
;; 3. Builds and byte-compiles packages
;; 4. Does NOT activate packages (that happens in normal mode)
;;
;; Usage: emacs --batch --eval "(setq user-emacs-directory \"/path/to/emacs-backpack/\")" -l ensure.el

;; Set backpack to sync mode BEFORE loading backpack.el
;; This variable is checked by backpack.el during initialization
(setq backpack-mode 'sync)

;; Set up load paths for base-packages before loading backpack.el
;; This is needed because backpack.el requires leaf at load time
(add-to-list 'load-path (expand-file-name "base-packages/leaf.el" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "base-packages/leaf-keywords.el" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Load backpack.el which sets up all the infrastructure
;; This will also install/build elpaca from base-packages if needed
(let ((backpack-file (expand-file-name "lisp/backpack.el" user-emacs-directory)))
  (load backpack-file nil nil nil t))

;; At this point, elpaca should be loaded from backpack--ensure-elpaca
;; Configure elpaca build steps for sync mode (build everything except activation)
(setq elpaca-build-steps backpack--sync-build-steps)

;; Load user configuration to get gear declarations
(let ((init-file (expand-file-name "init.el" backpack-user-dir)))
  (when (file-exists-p init-file)
    (load init-file t)))

;; Load all gears (which queues packages via :ensure)
(backpack-load-gear-files)

;; Wait for all packages to be installed/built
(elpaca-wait)

;; After elpaca-wait completes, run our finalization
;; (elpaca-after-init-hook doesn't run when using elpaca-wait in batch mode)
(when backpack--treesit-langs
  (message "")
  (message "Installing tree-sitter grammars...")
  (backpack--install-treesit-grammars))

(message "")
(message "========================================")
(message "Backpack synchronization complete!")
(message "You can now start Emacs normally.")
(message "========================================")
(kill-emacs 0)
