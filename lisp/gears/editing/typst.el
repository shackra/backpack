;; -*- lexical-binding: t; -*-

;; Declare tree-sitter languages needed by this gear
(when (backpack-treesit-was-asked 'typst)
  (backpack-treesit-recipe! typst
    :ts-mode 'typst-ts-mode
    :url "https://github.com/uben0/tree-sitter-typst"
    :ext "\\.typ\\'"
    :versions ((:until-abi 14 :revision "46cf4ded12ee974a70bf8457263b67ad7ee0379d"))))

(leaf typst-ts-mode
  :doc "markup language for people who find LaTeX hard to read"
  :ensure (typst-ts-mode
	   :host codeberg
	   :repo "meow_king/typst-ts-mode"
           :ref "278562d702de429f5c4369c007913ca0ef1584f3")
  :when (gearp! :editing typst)
  :mode "\\.typ\\'"
  :hook
  (typst-ts-mode-hook . visual-line-mode)
  (typst-ts-mode-hook .
		      (lambda ()
			(unless (gearp! :editing typst -display-line-numbers)
			  (display-line-numbers-mode +1)))))

(leaf eglot
  :doc "Language Server Protocol support for Typst via tinymist"
  :when (and (gearp! :editing typst) (gearp! :editing typst lsp))
  :doctor
  ("tinymist" . "an implementation of Language Server Protocol for Typst")
  :hook (typst-ts-mode-hook . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(typst-ts-mode . ("tinymist"))))
