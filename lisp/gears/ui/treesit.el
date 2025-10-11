(require 'backpack-pouch)

(leaf treesit-auto
  :doc "activate treesit everywhere"
  :unless (gearp! :ui -treesit)
  :ensure (treesit-auto :ref "016bd286a1ba4628f833a626f8b9d497882ecdf3")
  :global-minor-mode global-treesit-auto-mode
  :init
  (defun backpack--treesit-install-language-grammar-around (orig-fun lang &optional out-dir)
    "Ensure that all grammars are compiled and put on `backpack-tree-sitter-installation-directory`."
    (apply orig-fun lang (list (or out-dir backpack-tree-sitter-installation-directory))))
  :advice
  (:around treesit-install-language-grammar backpack--treesit-install-language-grammar-around)
  :custom
  (treesit-auto-install . 'prompt)
  (treesit-auto-langs . '()) ;; start fresh
  :config
  (add-to-list 'treesit-extra-load-path backpack-tree-sitter-installation-directory))
