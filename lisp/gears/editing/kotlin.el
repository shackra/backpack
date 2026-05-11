;; Declare tree-sitter languages needed by this gear
(when (backpack-treesit-was-asked 'kotlin "30.1")
  (backpack-treesit-recipe! kotlin
    :ts-mode 'kotlin-ts-mode
    :remap 'kotlin-mode
    :url "https://github.com/fwcd/tree-sitter-kotlin"
    :ext "\\.kt\\'"
    :versions ((:until-abi 14 :revision "f66d2908542e93c0204c6c241f794afe4e9cd5d1"))))

(leaf kotlin-mode
  :doc "null pointer exceptions, but make it concise"
  :ensure (kotlin-mode :ref "fddd747e5b4736e8b27a147960f369b86179ddff")
  :when (gearp! :editing kotlin)
  :hook
  ((kotlin-mode-hook kotlin-ts-mode-hook) . electric-pair-local-mode)
  ((kotlin-mode-hook kotlin-ts-mode-hook) .
   (lambda ()
     (toggle-truncate-lines +1)
     (unless (gearp! :editing kotlin -display-line-numbers)
       (display-line-numbers-mode +1)))))

(leaf kotlin-ts-mode
  :doc "tree-sitter support for Kotlin"
  :when (gearp! :editing kotlin)
  :unless (gearp! :editing kotlin -treesit)
  :emacs>= "30.1"
  :ensure (kotlin-ts-mode :ref "136d8d1fd3158fc5558aff866041c1935b574588" :host github :repo "emacsmirror/kotlin-ts-mode")
  :after kotlin-mode)

(leaf eglot
  :doc "Language Server Protocol support for Kotlin"
  :when (and (gearp! :editing kotlin) (gearp! :editing kotlin lsp))
  :doctor ("kotlin-language-server" . ("a language server for Kotlin built on top of the Kotlin compiler" required))
  :hook ((kotlin-mode-hook kotlin-ts-mode-hook) . eglot-ensure)
  :config
  ;; LSP protocol extensions note: kotlin-language-server uses standard LSP.
  ;; No non-standard protocol extensions documented.
  (add-to-list 'eglot-server-programs '(kotlin-mode . ("kotlin-language-server")))
  (add-to-list 'eglot-server-programs '(kotlin-ts-mode . ("kotlin-language-server"))))
