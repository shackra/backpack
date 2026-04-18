;; -*- lexical-binding: t; -*-
;;; backpack-sync.el --- Package synchronisation (build/activate) for Backpack  -*- lexical-binding: t; -*-

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

;; Elpaca install/build/ensure, mode-based setup, sync-mode form
;; filtering, and gc-mode advice.  `backpack ensure' and `backpack gc'
;; both depend on this module.

;;; Code:

;;; Variables defined in backpack.el (declared here for byte-compiler)

(defvar backpack-mode 'normal
  "Current operating mode for Backpack. Set before this file is loaded.")

(defvar backpack-emacs-dir nil
  "The path of the currently loaded Emacs configuration.")

(defvar backpack-nonessential-dir nil
  "Location where Backpack stores nonessential files.")

(defvar backpack-user-dir nil
  "Location of the user's private configuration.")

(declare-function backpack-sync-mode-p "backpack")
(declare-function backpack-normal-mode-p "backpack")
(declare-function backpack-gc-mode-p "backpack-gc")
(declare-function backpack--gc-collect-package "backpack-gc")
(declare-function backpack-load-gear-files "backpack")

;;; Packages that should be activated even in sync mode

(defvar backpack--enable-on-sync-packages nil
  "List of package symbols that should be activated even in sync mode.
This is useful for packages like treesit-auto that need to be active
during sync to install tree-sitter grammars.")

(defmacro backpack-enable-on-sync! (&rest packages)
  "Mark PACKAGES to be activated even in sync mode.
Call this macro BEFORE the leaf declaration for packages that need
to be active during sync (e.g., for installing tree-sitter grammars).

Example:
  (backpack-enable-on-sync! treesit-auto)
  (leaf treesit-auto
    :ensure t
    :config ...)"
  `(dolist (pkg ',packages)
     (cl-pushnew pkg backpack--enable-on-sync-packages)))

(defun backpack--activate-package (package-name)
  "Manually activate PACKAGE-NAME by loading its autoloads.
This is used in sync mode for packages marked with `backpack-enable-on-sync!'."
  (let* ((build-dir (expand-file-name (symbol-name package-name) elpaca-builds-directory))
         (autoloads-file (expand-file-name
                          (format "%s-autoloads.el" package-name)
                          build-dir)))
    (if (not (file-exists-p build-dir))
        (message "Backpack: WARNING - build dir for %s does not exist" package-name)
      (add-to-list 'load-path build-dir)
      (if (not (file-exists-p autoloads-file))
          (message "Backpack: WARNING - autoloads for %s not found" package-name)
        (load autoloads-file nil t)
        (condition-case err
            (require package-name)
          (error
           (message "Backpack: Failed to require %s: %S" package-name err)))))))

(defun backpack--activate-enable-on-sync-packages ()
  "Activate all packages marked with `backpack-enable-on-sync!'.
This should be called after packages are built but before their config runs."
  (dolist (pkg backpack--enable-on-sync-packages)
    (message "Backpack: Activating package for sync: %s" pkg)
    (backpack--activate-package pkg)))

;;; Build steps for each mode

(defvar backpack--sync-build-steps
  '(elpaca--clone
    elpaca--configure-remotes
    elpaca--checkout-ref
    elpaca--run-pre-build-commands
    elpaca--queue-dependencies
    elpaca--check-version
    elpaca--link-build-files
    elpaca--generate-autoloads-async
    elpaca--byte-compile
    elpaca--compile-info
    elpaca--install-info
    elpaca--add-info-path
    elpaca--run-post-build-commands)
  "Build steps for sync mode - excludes `elpaca--activate-package'.")

(defvar backpack--activation-only-steps
  '(elpaca--queue-dependencies elpaca--add-info-path elpaca--activate-package)
  "Steps for normal mode - only activate already-built packages.")

;;; Elpaca installation from base-packages

(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" backpack-nonessential-dir))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))

;; Location of elpaca in base-packages (bundled with Backpack)
(defvar backpack--elpaca-source-dir
  (expand-file-name "base-packages/elpaca/" backpack-emacs-dir)
  "Location of the bundled elpaca source in base-packages.")

(defun backpack--copy-directory-recursively (source dest)
  "Copy SOURCE directory recursively to DEST using pure Emacs Lisp.
This is platform-agnostic and doesn't rely on external tools."
  (unless (file-exists-p dest)
    (make-directory dest t))
  (dolist (file (directory-files source t "^[^.]"))
    (let ((dest-file (expand-file-name (file-name-nondirectory file) dest)))
      (cond
       ((file-directory-p file)
        (backpack--copy-directory-recursively file dest-file))
       (t
        (copy-file file dest-file t))))))

(defun backpack--elpaca-repo-dir ()
  "Return the elpaca repo directory path."
  (expand-file-name "elpaca/" elpaca-repos-directory))

(defun backpack--elpaca-build-dir ()
  "Return the elpaca build directory path."
  (expand-file-name "elpaca/" elpaca-builds-directory))

(defun backpack--elpaca-installed-p ()
  "Return non-nil if elpaca is already installed in the expected location."
  (let ((repo-dir (backpack--elpaca-repo-dir)))
    (and (file-exists-p repo-dir)
         (file-exists-p (expand-file-name "elpaca.el" repo-dir)))))

(defun backpack--install-elpaca-from-base-packages ()
  "Copy elpaca from base-packages to elpaca-repos-directory.
This uses the bundled elpaca instead of cloning from the internet."
  (let ((repo-dir (backpack--elpaca-repo-dir)))
    (message "Backpack: Installing elpaca from base-packages...")
    (make-directory elpaca-repos-directory t)
    (backpack--copy-directory-recursively backpack--elpaca-source-dir repo-dir)
    (message "Backpack: Elpaca source copied to %s" repo-dir)))

(defun backpack--build-elpaca ()
  "Build elpaca: byte-compile and generate autoloads.
This replicates what the elpaca installer does but without cloning."
  (let* ((repo-dir (backpack--elpaca-repo-dir))
         (build-dir (backpack--elpaca-build-dir))
         (default-directory repo-dir))
    (message "Backpack: Building elpaca...")
    (let ((emacs-exe (concat invocation-directory invocation-name)))
      (call-process emacs-exe nil nil nil
                    "-Q" "-L" "." "--batch"
                    "--eval" "(byte-recompile-directory \".\" 0 'force)"))
    (add-to-list 'load-path repo-dir)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo-dir)
    (make-directory build-dir t)
    (dolist (file (directory-files repo-dir t "\\.elc?\\'"))
      (let ((dest (expand-file-name (file-name-nondirectory file) build-dir)))
        (unless (file-exists-p dest)
          (if (fboundp 'make-symbolic-link)
              (condition-case nil
                  (make-symbolic-link file dest)
                (error (copy-file file dest t)))
            (copy-file file dest t)))))
    (message "Backpack: Elpaca built successfully")))

(defun backpack--ensure-elpaca ()
  "Ensure elpaca is installed and built from base-packages.
In sync mode, this will copy and build elpaca if needed.
In normal mode, this just loads elpaca if it's already built."
  (let ((repo-dir (backpack--elpaca-repo-dir))
        (build-dir (backpack--elpaca-build-dir)))
    (cond
     ((and (file-exists-p build-dir)
           (file-exists-p (expand-file-name "elpaca-autoloads.el" build-dir)))
      (add-to-list 'load-path build-dir)
      (require 'elpaca-autoloads nil t))
     ((and (backpack-sync-mode-p)
           (backpack--elpaca-installed-p))
      (backpack--build-elpaca)
      (add-to-list 'load-path build-dir)
      (require 'elpaca-autoloads nil t))
     ((backpack-sync-mode-p)
      (backpack--install-elpaca-from-base-packages)
      (backpack--build-elpaca)
      (add-to-list 'load-path build-dir)
      (require 'elpaca-autoloads nil t))
     ((file-exists-p repo-dir)
      (add-to-list 'load-path repo-dir)
      (when (file-exists-p (expand-file-name "elpaca-autoloads.el" repo-dir))
        (load (expand-file-name "elpaca-autoloads.el" repo-dir) nil t)))
     (t
      (when (backpack-normal-mode-p)
        (display-warning 'backpack
                         "Elpaca is not installed. Run 'backpack ensure' first."
                         :error))))))

;;; Package ref change detection

(defun backpack--ref-needs-update-p (repo-dir requested-ref)
  "Return non-nil if REPO-DIR's HEAD doesn't match REQUESTED-REF.
REQUESTED-REF can be a commit hash, tag, or branch name.
Uses `elpaca-process-call' for cross-platform git invocation."
  (when (and repo-dir requested-ref (file-exists-p repo-dir))
    (let ((default-directory repo-dir))
      (condition-case nil
          (let ((current-rev (string-trim
                              (cadr (elpaca-process-call "git" "rev-parse" "HEAD")))))
            (not (or (string-prefix-p requested-ref current-rev)
                     (string-prefix-p current-rev requested-ref)
                     (let ((resolved (elpaca-process-call "git" "rev-parse" requested-ref)))
                       (and (eq (car resolved) 0)
                            (string= current-rev
                                     (string-trim (cadr resolved))))))))
        (error t)))))

(defun backpack--recipe-force-rebuild-on-ref-change (recipe)
  "Modify RECIPE to force rebuild if the ref has changed.
In sync mode, checks if the requested :ref differs from the currently
checked out commit.  If so, returns build steps that include fetch,
checkout, and rebuild operations."
  (when (backpack-sync-mode-p)
    (let* ((package (plist-get recipe :package))
           (requested-ref (plist-get recipe :ref))
           (repo-dir (when package
                       (expand-file-name package elpaca-repos-directory))))
      (when (and requested-ref
                 (backpack--ref-needs-update-p repo-dir requested-ref))
        (message "Backpack: Ref changed for '%s', will fetch and rebuild" package)
        `(:build (elpaca--fetch
                  elpaca--checkout-ref
                  elpaca--run-pre-build-commands
                  elpaca--queue-dependencies
                  elpaca--check-version
                  elpaca--link-build-files
                  elpaca--generate-autoloads-async
                  elpaca--byte-compile
                  elpaca--compile-info
                  elpaca--install-info
                  elpaca--add-info-path
                  elpaca--run-post-build-commands))))))

(defun backpack--recipe-skip-unbuilt-in-normal-mode (recipe)
  "Modify RECIPE to skip building in normal mode if package isn't already built.
Returns a plist with :build set to activation-only steps for unbuilt packages."
  (when (backpack-normal-mode-p)
    (let* ((package (plist-get recipe :package))
           (build-dir (when package
                        (expand-file-name package elpaca-builds-directory)))
           (builtp (and build-dir (file-exists-p build-dir))))
      (unless builtp
        (message "Backpack: Package '%s' is not installed. Run 'backpack ensure'." package)
        '(:build nil)))))

;;; Elpaca mode configuration

(defun backpack--setup-elpaca-for-mode ()
  "Configure elpaca based on `backpack-mode'."
  (when (featurep 'elpaca)
    (cond
     ((backpack-sync-mode-p)
      (setq elpaca-build-steps backpack--sync-build-steps)
      (add-to-list 'elpaca-recipe-functions #'backpack--recipe-force-rebuild-on-ref-change))
     ((backpack-normal-mode-p)
      (add-to-list 'elpaca-recipe-functions #'backpack--recipe-skip-unbuilt-in-normal-mode)))))

(defun backpack--packages-need-sync-p ()
  "Return non-nil if packages need synchronization (installation/building).
This checks if the elpaca builds directory exists and has content."
  (not (and (file-exists-p elpaca-builds-directory)
            (directory-files elpaca-builds-directory nil "^[^.]" t))))

(defun backpack--check-packages-installed ()
  "Check if packages are installed, warn user if sync is needed."
  (when (and (backpack-normal-mode-p)
             (backpack--packages-need-sync-p))
    (display-warning
     'backpack
     "Packages are not installed. Run 'backpack ensure' to install them."
     :warning)))

(defun backpack--activate-packages ()
  "Activate all queued packages without attempting installation.
Used in normal mode when packages are already built."
  (when (and (backpack-normal-mode-p) (featurep 'elpaca))
    (backpack--check-packages-installed)
    (elpaca-process-queues)))

(defun backpack--sync-packages ()
  "Install and build all queued packages without activation.
Used in sync mode (`backpack ensure')."
  (when (and (backpack-sync-mode-p) (featurep 'elpaca))
    (backpack--setup-elpaca-for-mode)
    (elpaca-process-queues)))

;;; Elpaca advice hooks (applied after elpaca loads)

(with-eval-after-load 'elpaca
  (backpack--setup-elpaca-for-mode)

  (defun backpack--elpaca-gc-advice (orig-fn order &rest body)
    "Advice for `elpaca' macro to collect package names during gc mode.
In gc mode, just collect the package name without actually queuing."
    (let* ((order-val (if (and (consp order) (eq (car order) 'quote))
                          (cadr order)
                        order))
           (pkg-name (cond
                      ((symbolp order-val) order-val)
                      ((consp order-val) (car order-val))
                      (t nil))))
      (if (backpack-gc-mode-p)
          (when pkg-name
            (backpack--gc-collect-package pkg-name)
            nil)
        (apply orig-fn order body))))

  (advice-add 'elpaca--expand-declaration :around #'backpack--elpaca-gc-advice)

  (defun backpack--elpaca-skip-forms-in-sync-mode (orig-fn q)
    "Advice to skip running deferred forms in sync mode.
Packages in `backpack--enable-on-sync-packages' will still have their forms run."
    (if (backpack-sync-mode-p)
        (condition-case err
            (let ((filtered nil)
                  (original-forms (elpaca-q<-forms q)))
              (dolist (entry original-forms)
                (when (memq (car entry) backpack--enable-on-sync-packages)
                  (push entry filtered)))
              (let ((forms-cell (nthcdr 7 q)))
                (setcar forms-cell (nreverse filtered)))
              (funcall orig-fn q))
          (error
           (message "Backpack: Error in forms filtering: %S" err)
           (funcall orig-fn q)))
      (funcall orig-fn q)))

  (advice-add 'elpaca--finalize-queue :around #'backpack--elpaca-skip-forms-in-sync-mode))

(provide 'backpack-sync)
;;; backpack-sync.el ends here