;; Declare tree-sitter languages needed by this gear
(when (and (gearp! :editing css)
           (not (gearp! :editing css -treesit)))
  (backpack-treesit-recipe! css
    :ts-mode 'css-ts-mode
    :remap 'css-mode
    :versions ((:until-abi 14 :revision "4a9aab1668bf13d024710420648ef9a9ee6ccc17"))))

(leaf css
  :doc "making the web pretty since 1996"
  :when (gearp! :editing css)
  :mode "\\.css\\'"
  :hook
  ((css-mode-hook css-ts-mode-hook) . electric-pair-local-mode)
  ((css-mode-hook css-ts-mode-hook) .
   (lambda ()
     (toggle-truncate-lines +1)
     (unless (gearp! :editing css -display-line-numbers)
       (display-line-numbers-mode +1)))))

(leaf css-ts-mode
  :doc "tree-sitter support for CSS"
  :when (gearp! :editing css)
  :unless (gearp! :editing css -treesit)
  :after css)

(leaf eglot
  :doc "Language Server Protocol support for CSS"
  :when (and (gearp! :editing css) (gearp! :editing css lsp))
  :doctor ("vscode-css-language-server" . ("CSS language server from vscode-langservers-extracted" required))
  :hook ((css-mode-hook css-ts-mode-hook) . eglot-ensure)
  :config
  ;; LSP protocol extensions note: vscode-css-language-server supports
  ;; custom/editableRange. Same npm package as HTML LSP. Not yet implemented.
  (add-to-list 'eglot-server-programs '(css-mode . ("vscode-css-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(css-ts-mode . ("vscode-css-language-server" "--stdio"))))
