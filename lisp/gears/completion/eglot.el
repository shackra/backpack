(leaf eglot
  :doc "the Emacs client for the Language Server Protocol"
  :unless (gearp! :completion -eglot)
  :leaf-defer nil
  :url "https://www.gnu.org/software/emacs/manual/html_mono/eglot.html"
  :bind (:eglot-mode-map
	 ("C-c l a" . eglot-code-actions)
	 ("C-c l r" . eglot-rename)
	 ("C-c l f" . eglot-format))
  :hook
  (eglot-managed-mode-hook . (lambda ()
			       (setq-local eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
			       (remove-hook 'eldoc-documentation-functions #'eglot-hover-eldoc-function t))))
