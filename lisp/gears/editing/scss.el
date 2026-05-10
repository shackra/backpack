(leaf scss-mode
  :doc "SCSS syntax support for Emacs"
  :when (gearp! :editing scss)
  :ensure (scss-mode :ref "cf58dbec5394280503eb5502938f3b5445d1b53d")
  :mode "\\.scss\\'"
  :hook
  (scss-mode-hook . electric-pair-local-mode)
  (scss-mode-hook .
		  (lambda ()
		    (toggle-truncate-lines +1)
		    (unless (gearp! :editing scss -display-line-numbers)
		      (display-line-numbers-mode +1)))))

(leaf eglot
  :doc "Language Server Protocol support for SCSS"
  :when (and (gearp! :editing scss) (gearp! :editing scss lsp))
  :doctor ("vscode-css-language-server" . ("CSS language server from vscode-langservers-extracted" required))
  :hook (scss-mode-hook . eglot-ensure)
  :config
  ;; LSP protocol extensions note: vscode-css-language-server supports
  ;; custom/editableRange. Same npm package as HTML LSP. Not yet implemented.
  (add-to-list 'eglot-server-programs '(scss-mode . ("vscode-css-language-server" "--stdio"))))
