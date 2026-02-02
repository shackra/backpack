;;; gc.el --- Backpack garbage collection mode -*- lexical-binding: t; -*-
;;
;; This file implements the `backpack gc' command which:
;; 1. Scans current configuration to determine which packages are needed
;; 2. Compares against installed packages
;; 3. Removes orphaned packages that are no longer required
;;
;; Usage: emacs --batch --eval "(setq user-emacs-directory \"/path/to/emacs-backpack/\")" -l gc.el
;; For dry-run: emacs --batch --eval "(setq user-emacs-directory \"/path/to/emacs-backpack/\")" -l gc.el --eval "(setq backpack-gc-dry-run t)"

;; Check for dry-run flag (can be set via command line)
(defvar backpack-gc-dry-run nil
  "When non-nil, perform a dry run without deleting anything.")

;; Set up load paths for base-packages before loading backpack.el
(add-to-list 'load-path (expand-file-name "base-packages/leaf.el" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "base-packages/leaf-keywords.el" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Load backpack.el which sets up all the infrastructure
(let ((backpack-file (expand-file-name "lisp/backpack.el" user-emacs-directory)))
  (load backpack-file nil nil nil t))

;; Run garbage collection
(message "")
(message "========================================")
(message "Backpack Garbage Collection")
(message "========================================")
(message "")

(backpack-gc backpack-gc-dry-run)

(message "")
(kill-emacs 0)
