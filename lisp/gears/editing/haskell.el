;; Declare tree-sitter languages needed by this gear.
;; haskell is not in treesit-auto's default recipe list, so a custom recipe
;; is required.
(when (and (gearp! :editing haskell)
           (not (gearp! :editing haskell -treesit)))
  (backpack-treesit-recipe! haskell
    :ts-mode 'haskell-ts-mode
    :remap 'haskell-mode
    :url "https://github.com/tree-sitter/tree-sitter-haskell"
    :versions ((:until-abi 14 :revision "a50070d5bb5bd5c1281740a6102ecf1f4b0c4f19"))))

(leaf haskell-mode
  :doc "a monad is just a monoid in the category of endofunctors, what's the problem?"
  :ensure (haskell-mode :ref "2dd755a5fa11577a9388af88f385d2a8e18f7a8d")
  :when (gearp! :editing haskell)
  :hook
  ((haskell-mode-hook haskell-ts-mode-hook) . electric-pair-local-mode)
  ((haskell-mode-hook haskell-ts-mode-hook) . haskell-indentation-mode)
  ((haskell-mode-hook haskell-ts-mode-hook) . interactive-haskell-mode)
  ((haskell-mode-hook haskell-ts-mode-hook) .
   (lambda ()
     (toggle-truncate-lines +1)
     (unless (gearp! :editing haskell -display-line-numbers)
       (display-line-numbers-mode +1)))))

(leaf eglot
  :doc "Language Server Protocol support for haskell-mode"
  :when (and (gearp! :editing haskell) (gearp! :editing haskell lsp))
  :doctor
  ("haskell-language-server-wrapper" . "an integration point for ghcide and haskell-ide-engine, providing a single unified interface for editors")
  :hook ((haskell-mode-hook haskell-ts-mode-hook) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(haskell-mode . ("haskell-language-server-wrapper" "--lsp")))
  (add-to-list 'eglot-server-programs '(haskell-ts-mode . ("haskell-language-server-wrapper" "--lsp"))))

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
