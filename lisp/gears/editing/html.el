;; Declare tree-sitter languages needed by this gear
(when (backpack-treesit-was-asked 'html)
  (backpack-treesit-recipe! html
    :ts-mode 'html-ts-mode
    :remap '(mhtml-mode sgml-mode html-mode)
    :url "https://github.com/tree-sitter/tree-sitter-html"
    :ext "\\.html\\'"
    :versions ((:until-abi 14 :revision "73a3947324f6efddf9e17c0ea58d454843590cc0"))))

(leaf mhtml-mode
  :doc "the backbone of the world wide web"
  :when (gearp! :editing html)
  :mode "\\.html?\\'"
  :hook
  ((mhtml-mode-hook html-ts-mode-hook) . electric-pair-local-mode)
  ((mhtml-mode-hook html-ts-mode-hook) .
   (lambda ()
     (toggle-truncate-lines +1)
     (unless (gearp! :editing html -display-line-numbers)
       (display-line-numbers-mode +1)))))

(leaf web-mode
  :doc "major mode for editing web templates (PHP, JSX, Blade, etc.)"
  :when (gearp! :editing html web)
  :ensure (web-mode :ref "e93b3fb89fd6345a5ff59795bed712abd486200a"))

(leaf eglot
  :doc "Language Server Protocol support for HTML"
  :when (and (gearp! :editing html) (gearp! :editing html lsp))
  :doctor ("vscode-html-language-server" . ("HTML language server from vscode-langservers-extracted" required))
  :hook ((mhtml-mode-hook sgml-mode-hook html-ts-mode-hook) . eglot-ensure)
  :config
  ;; LSP protocol extensions note: vscode-html-language-server supports
  ;; custom/editableRange and custom/notevalexported. Not yet implemented.
  (add-to-list 'eglot-server-programs '(mhtml-mode . ("vscode-html-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(html-mode . ("vscode-html-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(html-ts-mode . ("vscode-html-language-server" "--stdio"))))
