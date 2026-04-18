;; -*- lexical-binding: t; -*-
;;; backpack-gc.el --- Orphaned package cleanup for Backpack  -*- lexical-binding: t; -*-

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

;; Find and remove packages that are installed (have build/repo
;; directories) but no longer referenced by the user's configuration.
;;
;; Usage from `bin/backpack gc':
;;
;;   emacs --batch -l gc.el
;;
;; Or interactively:  M-x backpack-gc  (dry-run)
;;                   M-x backpack-gc  C-u  (for real)

;;; Code:

(require 'backpack-pouch)
(require 'backpack-sync)

(defvar backpack--gc-mode nil
  "When non-nil, we're in garbage collection mode.
In this mode, we collect package names without actually installing them.")

(defvar backpack--queued-packages nil
  "List of package names that would be queued based on current configuration.
This is populated during gc mode.")

(defun backpack-gc-mode-p ()
  "Return non-nil if Backpack is in garbage collection mode."
  (eq backpack--gc-mode t))

(defun backpack--gc-collect-package (package-name)
  "Add PACKAGE-NAME to the list of queued packages during gc collection."
  (when (and package-name (symbolp package-name))
    (cl-pushnew package-name backpack--queued-packages)))

(defun backpack--get-installed-packages ()
  "Return a list of package names that are currently installed (have build dirs)."
  (when (file-exists-p elpaca-builds-directory)
    (mapcar #'intern
            (cl-remove-if
             (lambda (name) (member name '("." "..")))
             (directory-files elpaca-builds-directory nil "^[^.]")))))

(defun backpack--get-repo-packages ()
  "Return a list of package names that have repos cloned."
  (when (file-exists-p elpaca-repos-directory)
    (mapcar #'intern
            (cl-remove-if
             (lambda (name) (member name '("." "..")))
             (directory-files elpaca-repos-directory nil "^[^.]")))))

(defun backpack--get-package-dependencies (package-name)
  "Get the dependencies of PACKAGE-NAME by reading its main elisp file.
Returns a list of dependency package names (symbols)."
  (let* ((build-dir (expand-file-name (symbol-name package-name) elpaca-builds-directory))
         (repo-dir (expand-file-name (symbol-name package-name) elpaca-repos-directory))
         (pkg-name-str (symbol-name package-name))
         (main-file (or (let ((f (expand-file-name (concat pkg-name-str ".el") build-dir)))
                          (and (file-exists-p f) f))
                        (let ((f (expand-file-name (concat pkg-name-str ".el") repo-dir)))
                          (and (file-exists-p f) f))
                        (let ((f (expand-file-name (concat pkg-name-str "-pkg.el") build-dir)))
                          (and (file-exists-p f) f))
                        (let ((f (expand-file-name (concat pkg-name-str "-pkg.el") repo-dir)))
                          (and (file-exists-p f) f)))))
    (when main-file
      (with-temp-buffer
        (insert-file-contents main-file)
        (goto-char (point-min))
        (condition-case nil
            (if (string-suffix-p "-pkg.el" main-file)
                (let ((form (read (current-buffer))))
                  (when (eq (car form) 'define-package)
                    (mapcar #'car (nth 4 form))))
              (when (re-search-forward "^;+[ \t]*Package-Requires[ \t]*:[ \t]*" nil t)
                (let ((deps-str (buffer-substring-no-properties (point) (line-end-position))))
                  (forward-line 1)
                  (while (looking-at "^;+[ \t]+\\([^;].*\\)")
                    (setq deps-str (concat deps-str " " (match-string 1)))
                    (forward-line 1))
                  (condition-case nil
                      (mapcar #'car (read deps-str))
                    (error nil)))))
          (error nil))))))

(defun backpack--collect-all-dependencies (packages)
  "Collect all transitive dependencies for PACKAGES.
Returns a list of all packages including dependencies."
  (let ((all-packages (copy-sequence packages))
        (to-process (copy-sequence packages))
        (processed nil))
    (while to-process
      (let* ((pkg (pop to-process))
             (deps (backpack--get-package-dependencies pkg)))
        (push pkg processed)
        (dolist (dep deps)
          (unless (or (eq dep 'emacs)
                      (memq dep all-packages)
                      (memq dep processed))
            (push dep all-packages)
            (push dep to-process)))))
    all-packages))

(defun backpack--find-orphaned-packages ()
  "Find packages that are installed but not needed by current configuration.
Returns a list of orphaned package names."
  (let* ((installed (backpack--get-installed-packages))
         (needed-with-deps (backpack--collect-all-dependencies backpack--queued-packages)))
    (cl-set-difference installed needed-with-deps)))

(defun backpack--delete-package (package-name)
  "Delete PACKAGE-NAME's build and repo directories."
  (let ((build-dir (expand-file-name (symbol-name package-name) elpaca-builds-directory))
        (repo-dir (expand-file-name (symbol-name package-name) elpaca-repos-directory)))
    (when (file-exists-p build-dir)
      (message "Backpack GC: Deleting build directory for %s..." package-name)
      (delete-directory build-dir t))
    (when (file-exists-p repo-dir)
      (message "Backpack GC: Deleting repo directory for %s..." package-name)
      (delete-directory repo-dir t))))

(defun backpack--gc-delete-orphaned-packages (orphaned-packages)
  "Delete all ORPHANED-PACKAGES from disk."
  (dolist (pkg orphaned-packages)
    (backpack--delete-package pkg)))

(defun backpack--calculate-directory-size (directory)
  "Calculate the total size of DIRECTORY in bytes."
  (let ((total 0))
    (when (file-exists-p directory)
      (dolist (file (directory-files-recursively directory ".*" t))
        (unless (file-directory-p file)
          (setq total (+ total (or (file-attribute-size (file-attributes file)) 0))))))
    total))

(defun backpack--format-size (bytes)
  "Format BYTES as a human-readable string."
  (cond
   ((>= bytes (* 1024 1024 1024))
    (format "%.2f GB" (/ bytes (* 1024.0 1024.0 1024.0))))
   ((>= bytes (* 1024 1024))
    (format "%.2f MB" (/ bytes (* 1024.0 1024.0))))
   ((>= bytes 1024)
    (format "%.2f KB" (/ bytes 1024.0)))
   (t (format "%d bytes" bytes))))

;;;###autoload
(defun backpack-gc (&optional dry-run)
  "Remove orphaned packages that are no longer needed.
If DRY-RUN is non-nil, only report what would be deleted without deleting."
  (setq backpack--queued-packages nil)
  (setq backpack--gc-mode t)

  (let ((init-file (expand-file-name "init.el" backpack-user-dir)))
    (when (file-exists-p init-file)
      (load init-file t)))

  (backpack-load-gear-files)

  (setq backpack--gc-mode nil)

  (cl-pushnew 'elpaca backpack--queued-packages)

  (let ((orphaned (backpack--find-orphaned-packages)))
    (if (null orphaned)
        (message "Backpack GC: No orphaned packages found. Nothing to clean up.")
      (let ((total-size 0))
        (dolist (pkg orphaned)
          (let ((build-dir (expand-file-name (symbol-name pkg) elpaca-builds-directory))
                (repo-dir (expand-file-name (symbol-name pkg) elpaca-repos-directory)))
            (setq total-size (+ total-size
                                (backpack--calculate-directory-size build-dir)
                                (backpack--calculate-directory-size repo-dir)))))

        (message "")
        (message "Backpack GC: Found %d orphaned package(s):" (length orphaned))
        (dolist (pkg orphaned)
          (message "  - %s" pkg))
        (message "")
        (message "Total space to be freed: %s" (backpack--format-size total-size))
        (message "")

        (if dry-run
            (message "Backpack GC: Dry run - no packages were deleted.")
          (backpack--gc-delete-orphaned-packages orphaned)
          (message "")
          (message "Backpack GC: Deleted %d orphaned package(s), freed %s."
                   (length orphaned)
                   (backpack--format-size total-size)))))))

(provide 'backpack-gc)
;;; backpack-gc.el ends here