;; Declare tree-sitter languages needed by this gear
;; Note: hyprlang requires a custom recipe since it's not in treesit-auto by default
(when (and (gearp! :editing hyprland)
           (not (gearp! :ui -treesit)))
  (backpack-treesit-langs! hyprlang)
  ;; Add custom recipe for hyprlang since it's not in treesit-auto by default
  ;; This must be outside the leaf form so it runs immediately when the gear loads,
  ;; not deferred inside elpaca's forms (which are skipped in sync mode)
  (with-eval-after-load 'treesit-auto
    (add-to-list 'treesit-auto-recipe-list
                 (make-treesit-auto-recipe
                  :lang 'hyprlang
                  :ts-mode 'hyprlang-ts-mode
                  :url "https://github.com/tree-sitter-grammars/tree-sitter-hyprlang"
                  :ext "/hypr/.*\\.conf\\'"))))

(leaf hyprlang-ts-mode
  :doc "a major mode designed to provide enhanced editing support for Hyprland configuration files by leveraging Tree-Sitter"
  :when (gearp! :editing hyprland)
  :ensure (hyprlang-ts-mode :ref "4a2a257a237a4c15d1132b2ba3fdf040d7b44ef8")
  :custom (hyprlang-ts-mode-indent-offset . 2)
  :mode "/hypr/.*\\.conf\\'"
  :hook
  (hyprlang-ts-mode-hook . electric-pair-local-mode)
  (hyprlang-ts-mode-hook .
			 (lambda ()
			   (toggle-truncate-lines +1)
			   (unless (gearp! :editing hyprland -display-line-numbers)
			     (display-line-numbers-mode +1)))))

(leaf eglot
  :doc "Language Server Protocol support for hyprlang-ts-mode"
  :when (gearp! :editing hyprland lsp)
  :doctor ("hyprls" . "LSP server for Hyprland configuration files")
  :hook (hyprlang-ts-mode-hook . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(hyprlang-ts-mode . ("hyprls"))))
