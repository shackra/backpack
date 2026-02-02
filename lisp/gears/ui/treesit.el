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
      ;; In normal mode, don't auto-install - grammars should already be installed from sync
      (treesit-auto-install . `,(if (backpack-normal-mode-p) nil 'prompt))
      :config
      ;; Set treesit-auto-langs to only the languages declared by enabled gears
      (when backpack--treesit-langs
        (setq treesit-auto-langs backpack--treesit-langs))
      (add-to-list 'treesit-extra-load-path backpack-tree-sitter-installation-dir)
      :global-minor-mode global-treesit-auto-mode)
