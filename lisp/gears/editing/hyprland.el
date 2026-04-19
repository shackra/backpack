;; Declare tree-sitter languages needed by this gear.
;; hyprlang is not in treesit-auto's default recipe list, so a custom recipe
;; is required.  This must be outside the leaf form so it runs immediately
;; when the gear loads, not deferred inside elpaca's forms (which are skipped
;; in sync mode).
(when (and (gearp! :editing hyprland)
           (not (gearp! :ui -treesit)))
  (backpack-treesit-recipe! hyprlang
    :ts-mode 'hyprlang-ts-mode
    :url "https://github.com/tree-sitter-grammars/tree-sitter-hyprlang"
    :ext "/hypr/.*\\.conf\\'"
    :versions ((:until-abi 14 :revision "c9012d6dcaaa939f17c21e1fdb17b013d139e6b9"))))

(leaf hyprlang-ts-mode
  :doc "for when you need your tiling window manager config to look just right"
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
