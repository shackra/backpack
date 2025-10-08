(require 'backpack-pouch)

(leaf treesit-auto
  :doc "activate treesit everywhere"
  :unless (gearp! :ui -treesit)
  :ensure (treesit-auto :ref "016bd286a1ba4628f833a626f8b9d497882ecdf3")
  :global-minor-mode global-treesit-auto-mode
  :custom
  (treesit-auto-install . 'always)
  (treesit-auto-langs . '()) ;; start fresh
  :config
  (when (gearp! :programming hyprland)
    (add-to-list
     'treesit-auto-recipe-list
     (make-treesit-auto-recipe
      :lang 'hyprlang
      :ts-mode 'hyprlang-ts-mode
      :url "https://github.com/tree-sitter-grammars/tree-sitter-hyprlang"
      :ext "/hypr/.*\\.conf\\'"))
    (add-to-list 'treesit-auto-langs 'hyprlang)))
