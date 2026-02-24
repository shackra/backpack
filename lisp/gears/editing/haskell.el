;; Declare tree-sitter languages needed by this gear
(when (and (gearp! :editing haskell)
           (not (gearp! :editing haskell -treesit)))
  (backpack-treesit-langs! haskell)
  (with-eval-after-load 'treesit-auto
    (add-to-list 'treesit-auto-recipe-list
		 (make-treesit-auto-recipe
		  :lang 'haskell
		  :ts-mode 'haskell-ts-mode
		  :url "https://github.com/tree-sitter/tree-sitter-haskell"))))

(leaf haskell-mode
  :doc "A Haskell editing mode for Emacs"
  :ensure (haskell-mode :ref "2dd755a5fa11577a9388af88f385d2a8e18f7a8d")
  :when (gearp! :editing haskell)
  :hook
  (haskell-mode-hook . electric-pair-local-mode)
  (haskell-mode-hook .
		     (lambda ()
		       (toggle-truncate-lines +1)
		       (unless (gearp! :editing haskell -display-line-numbers)
			 (display-line-numbers-mode +1))))
  :config
  (leaf eglot
    :doc "Language Server Protocol support for haskell-mode"
    :when (gearp! :editing haskell lsp)
    :doctor
    ("haskell-language-server-wrapper" . "an integration point for ghcide and haskell-ide-engine, providing a single unified interface for editors")
    :hook (haskell-mode-hook . eglot-ensure)
    :config
    (add-to-list 'eglot-server-programs '(haskell-mode . ("haskell-language-server-wrapper" "--lsp"))))

  (unless (gearp! :editing haskell -treesit)
    (add-to-list 'major-mode-remap-alist '(haskell-mode . haskell-ts-mode))
    (setq haskell-ts-mode-hook haskell-mode-hook)))

(leaf haskell-ts-mode
  :doc "tree-sitter support for Haskell"
  :ensure (haskell-ts-mode :ref "bf143ee8382f09e0a68d775d80445065f32929c3")
  :unless (gearp! :editing haskell -treesit)
  :after haskell-mode)

(leaf ob-haskell
  :doc "Haskell source blocks in org-mode"
  :when (and (gearp! :editing haskell) (gearp! :editing org))
  :after org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages (append org-babel-load-languages '((haskell . t)))))
