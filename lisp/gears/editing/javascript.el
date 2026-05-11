;; Declare tree-sitter languages needed by this gear
(when (backpack-treesit-was-asked 'javascript)
  (backpack-treesit-recipe! javascript
    :ts-mode 'js-ts-mode
    :remap 'js-mode
    :url "https://github.com/tree-sitter/tree-sitter-javascript"
    :ext "\\.js\\'"
    :versions ((:until-abi 14 :revision "39798e26b6d4dbcee8e522b8db83f8b2df33a5ea"))))

(leaf js
  :doc "the language that keeps giving (and taking away semicolons)"
  :when (gearp! :editing javascript)
  :mode "\\.js\\'"
  :hook
  ((js-mode-hook js-ts-mode-hook) . electric-pair-local-mode)
  ((js-mode-hook js-ts-mode-hook) .
   (lambda ()
     (toggle-truncate-lines +1)
     (unless (gearp! :editing javascript -display-line-numbers)
       (display-line-numbers-mode +1)))))

(leaf js-ts-mode
  :doc "tree-sitter support for JavaScript"
  :when (gearp! :editing javascript)
  :unless (gearp! :editing javascript -treesit)
  :mode "\\.js\\'"
  :after js)

(leaf js2-mode
  :doc "improved JavaScript editing with AST-based analysis"
  :when (gearp! :editing javascript js2)
  :ensure (js2-mode :ref "e0c302872de4d26a9c1614fac8d6b94112b96307")
  :hook (js-mode-hook . js2-minor-mode))

(leaf eglot
  :doc "Language Server Protocol support for JavaScript"
  :when (and (gearp! :editing javascript) (gearp! :editing javascript lsp))
  :doctor ("typescript-language-server" . ("the LSP server for JavaScript and TypeScript" required))
  :hook ((js-mode-hook js-ts-mode-hook) . eglot-ensure))
