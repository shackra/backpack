;; -*- lexical-binding: t; -*-
;;; backpack-platform.el --- Platform and system detection for Backpack  -*- lexical-binding: t; -*-

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

;; Platform detection constants and predicates used throughout Backpack.
;; Loaded early so other modules can branch on the host OS.

;;; Code:

(defconst backpack-system
  (pcase system-type
    ('darwin '(macos bsd))
    ((or 'cygwin 'windows-nt 'ms-dos) '(windows))
    ((or 'gnu 'gnu/linux) '(gnu))
    ((or 'gnu/kfreebsd 'berkeley-unix) '(gnu bsd))
    ('android '(android)))
  "The operating system Emacs is running on.")

(defconst backpack--system-windows-p (eq 'windows (car backpack-system)))
(defconst backpack--system-macos-p (eq 'macos (car backpack-system)))
(defconst backpack--system-gnu-p (eq 'gnu (car backpack-system)))

(when (and backpack--system-gnu-p
	   (getenv-internal "WSLENV"))
  (add-to-list 'backpack-system 'wsl 'append))

(defconst backpack--system-wsl-p (memq 'wsl backpack-system)
  "Non-nil when Emacs is running inside Windows Subsystem for Linux.")

(push :system features)
(put :system 'subfeatures backpack-system)

(when backpack--system-windows-p
  (when-let* ((realhome
	       (and (null (getenv-internal "HOME"))
		    (getenv "USERPROFILE"))))
    (setenv "HOME" realhome)
    (setq abbreviated-home-dir nil)))

(defun backpack--windows-posix-bash-executable ()
  "Return bash.exe suitable for POSIX subprocesses on Windows, or nil.

Prefer Git for Windows and MSYS2 over the Store `WindowsApps' bash shim,
which mishandles `bash -c' argument passing for subprocesses."
  (when backpack--system-windows-p
    (or (catch 'backpack--got-bash
          (dolist (p (list (when-let ((pf (getenv "ProgramFiles")))
                             (expand-file-name "Git/bin/bash.exe" pf))
                           (when-let ((pfx86 (getenv "ProgramFiles(x86)")))
                             (expand-file-name "Git/bin/bash.exe" pfx86))
                           "c:/msys64/usr/bin/bash.exe"
                           "c:/msys64/bin/bash.exe"))
            (when (and p (file-exists-p p)
                       (not (string-match-p "[/\\\\]WindowsApps[/\\\\]" p)))
              (throw 'backpack--got-bash p))))
        (catch 'backpack--got-bash
          (dolist (dir (split-string (or (getenv "PATH") "") path-separator t))
            (let ((exe (expand-file-name "bash.exe" dir)))
              (when (and (file-exists-p exe)
                         (not (string-match-p "[/\\\\]WindowsApps[/\\\\]" exe)))
                (throw 'backpack--got-bash exe))))))))

(provide 'backpack-platform)
;;; backpack-platform.el ends here