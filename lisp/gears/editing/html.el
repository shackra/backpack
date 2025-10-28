(leaf sgml-mode
  :doc "major mode for editing HTML files"
  :when (gearp! :editing html)
  :hook
  (html-mode-hook . electric-pair-local-mode)
  (html-mode-hook . (lambda ()
		      (toggle-truncate-lines 1)
		      (unless (gearp! :editing html -display-line-numbers)
			(display-line-numbers-mode +1))))
  :config
  (leaf eglot
    :doc "Language Server Protocol support for HTML"
    :when (gearp! :editing html lsp)
    :hook
    (html-mode-hook . eglot-ensure)
    :doctor
    ("superhtml" . "HTML Validator, Formatter, LSP, and Templating Language Library")
    :config
    (add-to-list 'eglot-server-programs
		 '(html-mode . ("superhtml" "lsp")))))

(when (and (not (gearp! :editing html -treesit))
	   (gearp! :editing html))
  (with-eval-after-load 'treesit-auto
    (add-to-list 'treesit-auto-langs 'html))
  (add-to-list 'major-mode-remap-alist '(html-mode . html-ts-mode))
  (add-to-list 'major-mode-remap-alist '(mhtml-mode . html-ts-mode))
  (setq html-ts-mode-hook html-mode-hook))
