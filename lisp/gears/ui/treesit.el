;; Mark treesit-auto for activation in sync mode (needed for grammar installation)
;; This must come BEFORE the leaf declaration so the package is registered
;; before elpaca processes the queue
(unless (gearp! :ui -treesit)
  (backpack-enable-on-sync! treesit-auto))

(leaf treesit-auto
  :doc "activate treesit everywhere"
  :unless (gearp! :ui -treesit)
  :ensure (treesit-auto :ref "016bd286a1ba4628f833a626f8b9d497882ecdf3")
  :require t
  :advice
  (:around treesit-install-language-grammar
           (lambda (orig-fun lang &optional out-dir)
             "Ensure that all grammars are compiled and put on `backpack-tree-sitter-installation-dir'."
             (apply orig-fun lang (list (or out-dir backpack-tree-sitter-installation-dir)))))
  :init
  ;; In normal mode, don't auto-install - grammars should already be installed from sync
  ;; In sync mode, allow prompting (though we control installation via backpack)
  (setq treesit-auto-install (if (backpack-normal-mode-p) nil 'prompt))
  :config
  ;; Set treesit-auto-langs to only the languages declared by enabled gears
  (when backpack--treesit-langs
    (setq treesit-auto-langs backpack--treesit-langs))
  (add-to-list 'treesit-extra-load-path backpack-tree-sitter-installation-dir)
  ;; Only enable the global mode in normal mode, not during sync
  (unless (backpack-sync-mode-p)
    (global-treesit-auto-mode)))
