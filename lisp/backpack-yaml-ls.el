;; -*- lexical-binding: t; -*-

;;; backpack-yaml-ls.el --- yaml-language-server LSP protocol extensions for Eglot

;; This library extends Eglot with support for the non-standard LSP protocol
;; extensions implemented by yaml-language-server:
;;
;;   https://github.com/redhat-developer/yaml-language-server#language-server-protocol-extensions
;;
;; It is loaded on demand from the yaml gear when the `lsp' flag is active and
;; the user has not opted out with `-yaml-ls-ext'.  It must NOT be loaded
;; unconditionally from backpack.el.
;;
;; Extension overview:
;;
;;   Client → Server  yaml/supportSchemaSelection (notification, post-connect)
;;   Server → Client  yaml/schema/store/initialized (notification, handled)
;;   Client → Server  yaml/get/all/jsonSchemas (request)
;;   Client → Server  yaml/get/jsonSchema (request)
;;
;; Public commands:
;;
;;   M-x eglot-yaml-show-all-schemas
;;   M-x eglot-yaml-show-schemas-for-buffer

(require 'eglot)
(require 'cl-lib)

(defvar backpack-yaml-ls--ready-servers (make-hash-table :test 'eq :weakness 'key)
  "Hash table mapping yaml-ls servers to schema-store readiness.")

;;; ---------------------------------------------------------------------------
;;; Detection
;;; ---------------------------------------------------------------------------

(defun backpack-yaml-ls--server-p (server)
  "Return non-nil if SERVER is a yaml-language-server process."
  (cl-find "yaml-language-server"
           (process-command (jsonrpc--process server))
           :test #'string-match))

;;; ---------------------------------------------------------------------------
;;; Post-connect notification: yaml/supportSchemaSelection
;;; ---------------------------------------------------------------------------

(defun backpack-yaml-ls--notify-schema-support (server)
  "Send yaml/supportSchemaSelection to SERVER if it is yaml-language-server.
Added to `eglot-connect-hook', which fires after the LSP :initialized
handshake completes -- the correct moment for post-connect notifications."
  (when (backpack-yaml-ls--server-p server)
    (jsonrpc-notify server :yaml/supportSchemaSelection nil)))

(add-hook 'eglot-connect-hook #'backpack-yaml-ls--notify-schema-support)

;;; ---------------------------------------------------------------------------
;;; Incoming notification: yaml/schema/store/initialized
;;; ---------------------------------------------------------------------------

(cl-defmethod eglot-handle-notification
  (server (_method (eql yaml/schema/store/initialized)) &rest _params)
  "Handle yaml-language-server schema store initialized notification.
The server sends this once it has finished loading schemas from the
schema store.  We acknowledge it and tell the user."
  (when (backpack-yaml-ls--server-p server)
    (puthash server t backpack-yaml-ls--ready-servers)
    (message "[eglot] yaml: schema store initialized")))

;;; ---------------------------------------------------------------------------
;;; Requests (private)
;;; ---------------------------------------------------------------------------

(defun backpack-yaml-ls--get-all-schemas ()
  "Request all schemas from yaml-language-server for the current buffer.
Sends yaml/get/all/jsonSchemas with the current document URI.
Returns a vector of plists; each plist has keys :uri, :name,
:description, :usedForCurrentFile, :fromStore."
  (let ((server (eglot-current-server)))
    (unless server
      (user-error "No eglot server managing this buffer"))
    (unless (backpack-yaml-ls--server-p server)
      (user-error "Active eglot server is not yaml-language-server"))
    (unless (gethash server backpack-yaml-ls--ready-servers)
      (user-error "Schema store not ready yet; wait for '[eglot] yaml: schema store initialized'"))
    (eglot--request server
                    :yaml/get/all/jsonSchemas
                    (eglot-path-to-uri (buffer-file-name)))))

(defun backpack-yaml-ls--get-schemas-for-buffer ()
  "Request schemas applied to the current buffer from yaml-language-server.
Sends yaml/get/jsonSchema with the current document URI.
Returns a vector of plists; each plist has keys :uri, :name,
:description."
  (let ((server (eglot-current-server)))
    (unless server
      (user-error "No eglot server managing this buffer"))
    (unless (backpack-yaml-ls--server-p server)
      (user-error "Active eglot server is not yaml-language-server"))
    (unless (gethash server backpack-yaml-ls--ready-servers)
      (user-error "Schema store not ready yet; wait for '[eglot] yaml: schema store initialized'"))
    (eglot--request server
                    :yaml/get/jsonSchema
                    (eglot-path-to-uri (buffer-file-name)))))

;;; ---------------------------------------------------------------------------
;;; Display helpers
;;; ---------------------------------------------------------------------------

(defun backpack-yaml-ls--buffer-name (suffix)
  "Return a display buffer name for yaml-ls output identified by SUFFIX."
  (format "*eglot-yaml-%s: %s*"
          suffix
          (file-name-nondirectory (or (buffer-file-name) (buffer-name)))))

(defun backpack-yaml-ls--display (buf-name schemas used-label all-p)
  "Populate and display a read-only buffer BUF-NAME with SCHEMAS.
USED-LABEL is the header line.  ALL-P non-nil means include the
:usedForCurrentFile and :fromStore columns."
  (let ((source-buf (current-buffer)))
    (with-current-buffer (get-buffer-create buf-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize used-label 'face 'bold))
        (insert "\n\n")
        (if (or (null schemas) (zerop (length schemas)))
            (insert "(no schemas)\n")
          (cl-loop
           for schema across schemas
           for uri         = (plist-get schema :uri)
           for name        = (plist-get schema :name)
           for description = (plist-get schema :description)
           for used        = (plist-get schema :usedForCurrentFile)
           for from-store  = (plist-get schema :fromStore)
           do
           (insert (propertize (or name uri) 'face 'font-lock-keyword-face))
           (when (and all-p (eq used t))
             (insert (propertize "  [active]" 'face 'font-lock-string-face)))
           (insert "\n")
           (when description
             (insert "  " description "\n"))
           (insert "  " (propertize uri 'face 'font-lock-comment-face))
           (when all-p
             (insert
              (format "  store:%s" (if (eq from-store t) "yes" "no"))))
           (insert "\n\n")))
        (goto-char (point-min)))
      (special-mode)
      (display-buffer (current-buffer)
                      '((display-buffer-reuse-window
                         display-buffer-pop-up-window))))))

;;; ---------------------------------------------------------------------------
;;; Public commands
;;; ---------------------------------------------------------------------------

(defun eglot-yaml-show-all-schemas ()
  "Display all schemas known to yaml-language-server for the current buffer.
Shows each schema's name, URI, description, whether it is currently
applied to this file, and whether it originates from the schema store.
Requires an active eglot connection to yaml-language-server."
  (interactive)
  (let* ((schemas  (backpack-yaml-ls--get-all-schemas))
         (buf-name (backpack-yaml-ls--buffer-name "all-schemas")))
    (backpack-yaml-ls--display
     buf-name schemas
     (format "All schemas known to yaml-language-server (%s)"
             (file-name-nondirectory (or (buffer-file-name) (buffer-name))))
     t)))

(defun eglot-yaml-show-schemas-for-buffer ()
  "Display schemas applied to the current YAML buffer by yaml-language-server.
Requires an active eglot connection to yaml-language-server."
  (interactive)
  (let* ((schemas  (backpack-yaml-ls--get-schemas-for-buffer))
         (buf-name (backpack-yaml-ls--buffer-name "schemas")))
    (backpack-yaml-ls--display
     buf-name schemas
     (format "Schemas active for %s"
             (file-name-nondirectory (or (buffer-file-name) (buffer-name))))
     nil)))

;;; ---------------------------------------------------------------------------

(provide 'backpack-yaml-ls)

;;; backpack-yaml-ls.el ends here
