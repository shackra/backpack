(leaf css-mode
  :doc "major mode for editing CSS files"
  :when (gearp! :editing css)
  :hook
  (css-mode-hook . electric-pair-local-mode)
  (css-mode-hook . (lambda ()
		     (toggle-truncate-lines 1)
		     (unless (gearp! :editing css -display-line-numbers)
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
    ("tailwindcss-language-server" . "Language Server Protocol implementation for Tailwind CSS, used by Tailwind CSS IntelliSense for VS Code")
    :config
    (add-to-list 'eglot-server-programs
		 `((css-mode css-ts-mode) . ,(eglot-alternatives
					      '(("vscode-css-language-server" "--stdio")
						("css-languageserver" "--stdio")
						("tailwindcss-language-server" "--stdio")))))))

(when (and (not (gearp! :editing css -treesit))
	   (gearp! :editing css))
  (with-eval-after-load 'treesit-auto
    (add-to-list 'treesit-auto-langs 'css))
  (add-to-list 'major-mode-remap-alist '(css-mode . css-ts-mode))
  (setq css-ts-mode-hook css-mode-hook))
