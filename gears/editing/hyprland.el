(require 'backpack-pouch)

(leaf hyprlang-ts-mode
  :doc "a major mode designed to provide enhanced editing support for Hyprland configuration files by leveraging Tree-Sitter"
  :when (gearp! :editing hyprland)
  :ensure (hyprlang-ts-mode :ref "4a2a257a237a4c15d1132b2ba3fdf040d7b44ef8")
  :custom (hyprlang-ts-mode-indent-offset . 2)
  :config
  (add-to-list 'treesit-auto-langs 'hyprlang)
  (add-to-list 'treesit-auto-recipe-list
	       (make-treesit-auto-recipe
		:lang 'hyprlang
		:ts-mode 'hyprlang-ts-mode
		:url "https://github.com/tree-sitter-grammars/tree-sitter-hyprlang"
		:ext "/hypr/.*\\.conf\\'"))

  (leaf eglot
    :doc "Language Server Protocol support"
    :when (gearp! :editing hyprland lsp)
    :doctor ("hyprls" . "LSP server for Hyprland configuration files")
    :hook (hyprlang-ts-mode-hook . eglot-ensure)
    :config
    (add-to-list 'eglot-server-programs '(hyprlang-ts-mode . ("hyprls")))))
