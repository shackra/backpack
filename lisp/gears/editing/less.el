(leaf less-css-mode
  :doc "major mode for editing LESS files"
  :when (gearp! :editing less)
  :hook
  (less-css-mode-hook . electric-pair-local-mode)
  (less-css-mode-hook . (lambda ()
			  (toggle-truncate-lines 1)
			  (unless (gearp! :editing less -display-line-numbers)
			    (display-line-numbers-mode +1))))
  :config
  (leaf eglot
    :doc "Language Server Protocol support for CSS"
    :when (gearp! :editing css lsp)
    :hook
    (css-mode-hook . eglot-ensure)
    :doctor
    ("vscode-css-language-server" . "CSS, LESS & SCSS language service extracted from VSCode to be reused")
    ("css-languageserver" . "same as vscode-css-language-server?")
    :config
    (add-to-list 'eglot-server-programs
		 `(less-css-mode . ,(eglot-alternatives
				     '(("vscode-css-language-server" "--stdio")
				       ("css-languageserver" "--stdio")))))))
