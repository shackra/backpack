;; -*- lexical-binding: t; -*-
;;; backpack-defaults.el --- Sensible default settings for Backpack  -*- lexical-binding: t; -*-

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

;; Global defaults for native compilation, security, warnings, file
;; locations, custom-file, and backup directories.  Loaded early by
;; backpack.el so the settings take effect before packages initialise.

;;; Code:

(require 'backpack-platform)

;;; Directory variables (defined in backpack.el, declared here for byte-compiler)
(defvar backpack-emacs-dir nil)
(defvar backpack-cache-dir nil)
(defvar backpack-data-dir nil)
(defvar backpack-nonessential-dir nil)
(defvar backpack-state-dir nil)
(defvar backpack-user-dir nil)

;;; CLI settings
(when noninteractive
  (setq make-backup-files nil)
  (setq enable-dir-local-variables nil)
  (setq case-fold-search nil)
  (setq delete-by-moving-to-trash nil))

;;; Don't litter `backpack-emacs-dir'/$HOME
(setq user-emacs-directory backpack-cache-dir)

(setq server-auth-dir (file-name-concat backpack-emacs-dir "server/"))

(setq desktop-dirname  (file-name-concat backpack-state-dir "desktop")
      pcache-directory (file-name-concat backpack-cache-dir "pcache/"))

(setq custom-file (file-name-concat backpack-user-dir "custom.el"))

(setq backup-directory-alist `(("." . ,backpack-nonessential-dir)))

(define-advice en/disable-command (:around (fn &rest args) write-to-data-dir)
  "Save safe-local-variables to `custom-file' instead of `user-init-file'."
  (let ((user-init-file custom-file))
    (apply fn args)))

;;; Native compilation support
(when (boundp 'native-comp-eln-load-path)
  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln/" backpack-cache-dir))

  (setq native-comp-async-report-warnings-errors init-file-debug
        native-comp-warning-on-missing-source init-file-debug)

  (unless (boundp 'native-comp-deferred-compilation-deny-list)
    (defvaralias 'native-comp-deferred-compilation-deny-list 'native-comp-jit-compilation-deny-list))

  (define-advice comp-effective-async-max-jobs (:before (&rest _) set-default-cpus)
    (and (null comp-num-cpus)
         (zerop native-comp-async-jobs-number)
         (setq comp-num-cpus
               (max 1 (/ (num-processors) (if noninteractive 1 4))))))

  (define-advice comp-run-async-workers (:around (fn &rest args) dont-litter-tmpdir)
    (let ((temporary-file-directory (expand-file-name "comp/" backpack-cache-dir)))
      (make-directory temporary-file-directory t)
      (apply fn args)))

  (with-eval-after-load 'comp
    (require 'comp-run nil t)
    (mapc (apply-partially #'add-to-list 'native-comp-deferred-compilation-deny-list)
          (list "/seq-tests\\.el\\'"
                "/emacs-jupyter.*\\.el\\'"
                "/evil-collection-vterm\\.el\\'"
                "/vterm\\.el\\'"
                "/with-editor\\.el\\'"))))

;;; Reduce unnecessary/unactionable warnings/logs
(setq ad-redefinition-action 'accept)

(setq warning-suppress-types '((defvaralias) (lexical-binding)))

(setq warning-inhibit-types '((files missing-lexbind-cookie)))

(setq debug-on-error init-file-debug
      jka-compr-verbose init-file-debug)

;;; Stricter security defaults
(setq gnutls-verify-error noninteractive
      gnutls-algorithm-priority
      (when (boundp 'libgnutls-version)
        (concat "SECURE128:+SECURE192:-VERS-ALL"
                (if (and (not backpack--system-windows-p)
                         (>= libgnutls-version 30605))
                    ":+VERS-TLS1.3")
                ":+VERS-TLS1.2"))
      gnutls-min-prime-bits 3072
      tls-checktrust gnutls-verify-error
      tls-program '("openssl s_client -connect %h:%p -CAfile %t -nbio -no_ssl3 -no_tls1 -no_tls1_1 -ign_eof"
                    "gnutls-cli -p %p --dh-bits=3072 --ocsp --x509cafile=%t \
--strict-tofu --priority='SECURE192:+SECURE128:-VERS-ALL:+VERS-TLS1.2:+VERS-TLS1.3' %h"
                    "gnutls-cli -p %p %h"))

;;; Package managers
(setq package-enable-at-startup nil)

(provide 'backpack-defaults)
;;; backpack-defaults.el ends here