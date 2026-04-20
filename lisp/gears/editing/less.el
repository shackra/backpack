(leaf less-css-mode
  :doc "Less CSS editing support"
  :when (gearp! :editing less)
  :ensure (less-css-mode :ref "c78b88ff9af245daf05d45f5adf4befc11c1d4ef")
  :mode "\\.less\\'"
  :hook
  (less-css-mode-hook . electric-pair-local-mode)
  (less-css-mode-hook .
		      (lambda ()
			(toggle-truncate-lines +1)
			(unless (gearp! :editing less -display-line-numbers)
			  (display-line-numbers-mode +1)))))

(leaf eglot
  :doc "Language Server Protocol support for Less"
  :when (and (gearp! :editing less) (gearp! :editing less lsp))
  :doctor ("vscode-css-language-server" . ("CSS language server from vscode-langservers-extracted" required))
  :hook (less-css-mode-hook . eglot-ensure)
  :config
  ;; LSP protocol extensions note: vscode-css-language-server supports
  ;; custom/editableRange. Same npm package as HTML LSP. Not yet implemented.
  (add-to-list 'eglot-server-programs '(less-css-mode . ("vscode-css-language-server" "--stdio"))))
