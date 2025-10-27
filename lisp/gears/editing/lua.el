(leaf lua-mode
  :doc "A major-mode for editing Lua scripts"
  :ensure (lua-mode :ref "2f6b8d7a6317e42c953c5119b0119ddb337e0a5f")
  :when (gearp! :editing lua)
  :hook
  (lua-mode-hook . electric-pair-local-mode)
  (lua-mode-hook .
		 (lambda ()
		   (toggle-truncate-lines +1)
		   (unless (gearp! :editing lua -display-line-numbers)
		     (display-line-numbers-mode +1))))
  :config
  (leaf eglot
    :doc "Language Server Protocol support for lua-mode"
    :when (gearp! :editing lua lsp)
    :doctor
    ("lua-language-server" . "provides various language features for Lua to make development easier and faster")
    :hook (lua-mode-hook . eglot-ensure)
    :config
    (add-to-list 'eglot-server-programs '(lua-mode . ("lua-language-server"))))

  (unless (gearp! :editing lua -treesit)
    (add-to-list 'treesit-auto-langs 'lua)
    (add-to-list 'major-mode-remap-alist '(lua-mode . lua-ts-mode))
    (setq lua-ts-mode-hook lua-mode-hook)))
