;;; backpack-buffers.el --- Smart buffer switching for Backpack  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jorge Javier Araya Navarro and Backpack contributors

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

;; Intelligent buffer management inspired by Doom Emacs
;; (https://github.com/doomemacs/doomemacs).
;;
;; The "real buffer" concept, extensible hook architecture
;; (real/unreal buffer functions), fallback buffer protection, and
;; kill-buffer advice pattern are adapted from Doom's buffer library
;; (lisp/lib/buffers.el).  Combined here with Emacs 28+'s
;; `switch-to-prev-buffer-skip' for buffer cycling, which Doom does
;; not use.

;;; Code:

(require 'cl-lib)

;;;; Configuration variables

(defvar backpack-fallback-buffer-name "*scratch*"
  "Name of the fallback buffer.
This buffer is always kept alive and used as a last resort when no
other real buffer is available.")

(defvar backpack-real-buffer-modes
  '(dired-mode comint-mode eshell-mode shell-mode
    term-mode vterm-mode eat-mode)
  "Major modes always considered real.
Buffers with these modes are treated as real even if they are not
visiting a file.  Inspired by `doom-real-buffer-modes'.")

(defvar backpack-real-buffer-functions '()
  "Hook of functions returning non-nil if a buffer is real.
Each function receives one argument: the buffer.  If any function
returns non-nil, the buffer is considered real.
Inspired by `doom-real-buffer-functions'.")

(defvar backpack-unreal-buffer-functions
  '(minibufferp backpack-special-buffer-p backpack-non-file-visiting-buffer-p)
  "Hook of functions returning non-nil if a buffer is unreal.
Each function receives one argument: the buffer.  A buffer is
considered unreal only when none of the higher-priority checks
(buffer-local override, mode whitelist, `backpack-real-buffer-functions')
match AND at least one function here returns non-nil.
Inspired by `doom-unreal-buffer-functions'.")

;;;; Buffer classification

(defvar-local backpack-real-buffer-p nil
  "Buffer-local override for real buffer classification.
Set to a non-nil value to force a buffer to be considered real,
or to the symbol `unreal' to force it to be considered unreal.")

(defun backpack-special-buffer-p (buf)
  "Return non-nil if BUF name starts with `*'."
  (string-prefix-p "*" (buffer-name buf)))

(defun backpack-non-file-visiting-buffer-p (buf)
  "Return non-nil if BUF has no associated file."
  (not (buffer-file-name buf)))

(defun backpack-real-buffer-p (buf)
  "Return non-nil if BUF is a \"real\" buffer worth switching to.
A buffer is real if any of these conditions hold (checked in order):

1. Its buffer-local `backpack-real-buffer-p' variable is non-nil
   (but not the symbol `unreal').
2. Its major mode is in `backpack-real-buffer-modes'.
3. Any function in `backpack-real-buffer-functions' returns non-nil.
4. No function in `backpack-unreal-buffer-functions' returns non-nil.

Inspired by `doom-real-buffer-p' from Doom Emacs."
  (when-let ((buf (get-buffer buf)))
    (let ((override (buffer-local-value 'backpack-real-buffer-p buf)))
      (cond
       ;; Buffer-local override: unreal
       ((eq override 'unreal) nil)
       ;; Buffer-local override: real
       (override t)
       ;; Mode whitelist
       ((memq (buffer-local-value 'major-mode buf)
              backpack-real-buffer-modes)
        t)
       ;; Any real-buffer function says yes
       ((run-hook-with-args-until-success
         'backpack-real-buffer-functions buf)
        t)
       ;; None of the unreal functions say yes
       (t
        (not (run-hook-with-args-until-success
              'backpack-unreal-buffer-functions buf)))))))

;;;; Fallback buffer

(defun backpack-fallback-buffer ()
  "Return the fallback buffer, creating it if needed.
Inspired by `doom-fallback-buffer' from Doom Emacs."
  (get-buffer-create backpack-fallback-buffer-name))

(defun backpack-protect-fallback-buffer-h ()
  "Prevent killing the fallback buffer.
Intended for use in `kill-buffer-query-functions'.
Inspired by `doom-protect-fallback-buffer-h' from Doom Emacs."
  (not (eq (current-buffer) (backpack-fallback-buffer))))

;;;; Kill advice with same-mode priority

(defun backpack--find-buffer-with-mode (mode &optional exclude-buf)
  "Find the first real buffer with major MODE, excluding EXCLUDE-BUF."
  (cl-loop for buf in (buffer-list)
           when (and (not (eq buf exclude-buf))
                     (eq (buffer-local-value 'major-mode buf) mode)
                     (backpack-real-buffer-p buf))
           return buf))

(defun backpack--switch-to-fallback-buffer-maybe-a ()
  "Advice for `kill-current-buffer' that picks a smarter replacement.
When killing a buffer, this advice:
1. Prevents killing the fallback buffer.
2. Prompts before killing modified file-visiting buffers.
3. Switches to a buffer with the same major mode if available.
4. Falls back to any real buffer, then to the fallback buffer.

Inspired by `doom--switch-to-fallback-buffer-maybe-a' from Doom Emacs,
extended with same-mode priority."
  (let ((buf (current-buffer)))
    (cond
     ;; Don't kill fallback buffer
     ((eq buf (backpack-fallback-buffer))
      (message "Won't kill the fallback buffer")
      t)
     ;; Proceed with smart switching
     (t
      (when (and (buffer-modified-p buf)
                 (buffer-file-name buf)
                 (not (yes-or-no-p
                       (format "Buffer %s modified; kill anyway? "
                               (buffer-name buf)))))
        (user-error "Aborted"))
      (let* ((mode (buffer-local-value 'major-mode buf))
             (same-mode-buf (backpack--find-buffer-with-mode mode buf))
             (target (or same-mode-buf
                         (cl-loop for b in (buffer-list)
                                  when (and (not (eq b buf))
                                            (backpack-real-buffer-p b))
                                  return b)
                         (backpack-fallback-buffer))))
        ;; Switch all windows showing this buffer
        (dolist (win (get-buffer-window-list buf nil t))
          (set-window-buffer win target))
        (kill-buffer buf)
        t)))))

;;;; Buffer cycling via switch-to-prev-buffer-skip (Emacs 28+)

(setq switch-to-prev-buffer-skip
      (lambda (_win buf _bury-or-kill)
        (not (backpack-real-buffer-p buf))))

;;;; Frame buffer predicate

(add-to-list 'default-frame-alist
             '(buffer-predicate . backpack-real-buffer-p))

;;;; Keybinding

;; Ensure C-x k always calls kill-current-buffer (where our advice lives).
;; Default Emacs binds C-x k to kill-buffer which prompts and bypasses advice.
(global-set-key (kbd "C-x k") #'kill-current-buffer)

;;;; Activate

(add-hook 'kill-buffer-query-functions #'backpack-protect-fallback-buffer-h)
(advice-add 'kill-current-buffer :before-until
            #'backpack--switch-to-fallback-buffer-maybe-a)

(provide 'backpack-buffers)
;;; backpack-buffers.el ends here
