;; Declare tree-sitter languages needed by this gear
(when (and (gearp! :editing lua)
           (not (gearp! :editing lua -treesit)))
  (backpack-treesit-langs! lua))

(leaf lua-mode
  :doc "arrays start at 1 and we're not sorry"
  :ensure (lua-mode :ref "2f6b8d7a6317e42c953c5119b0119ddb337e0a5f")
  :when (gearp! :editing lua)
  :hook
  ((lua-mode-hook lua-ts-mode-hook) . electric-pair-local-mode)
  ((lua-mode-hook lua-ts-mode-hook) .
   (lambda ()
     (toggle-truncate-lines +1)
     (unless (gearp! :editing lua -display-line-numbers)
       (display-line-numbers-mode +1)))))

(leaf eglot
  :doc "Language Server Protocol support for lua-mode"
  :when (and (gearp! :editing lua) (gearp! :editing lua lsp))
  :doctor
  ("lua-language-server" . "provides various language features for Lua to make development easier and faster")
  :hook ((lua-mode-hook lua-ts-mode-hook) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(lua-mode . ("lua-language-server")))
  (add-to-list 'eglot-server-programs '(lua-ts-mode . ("lua-language-server"))))
