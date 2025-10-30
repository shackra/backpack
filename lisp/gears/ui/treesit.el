(leaf treesit-auto
  :doc "activate treesit everywhere"
  :unless (gearp! :ui -treesit)
  :ensure (treesit-auto :ref "016bd286a1ba4628f833a626f8b9d497882ecdf3")
  :advice
  (:around treesit-install-language-grammar
	   (lambda (orig-fun lang &optional out-dir)
	     "Ensure that all grammars are compiled and put on `backpack-tree-sitter-installation-dir'."
	     (apply orig-fun lang (list (or out-dir backpack-tree-sitter-installation-dir)))))
  :custom
  (treesit-auto-install . 'prompt)
  :config
  (add-to-list 'treesit-extra-load-path backpack-tree-sitter-installation-dir)
  :global-minor-mode global-treesit-auto-mode)
