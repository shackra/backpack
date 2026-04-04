;; Declare tree-sitter languages needed by this gear
(when (and (gearp! :editing yaml)
           (not (gearp! :editing yaml -treesit)))
  (backpack-treesit-langs! yaml)

  (add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode)))

(leaf yaml-mode
  :doc "indentation-sensitive and ready to ruin your CI pipeline"
  :ensure (yaml-mode :ref "d91f878729312a6beed77e6637c60497c5786efa")
  :when (gearp! :editing yaml)
  :mode "\\.ya?ml\\'"
  :hook
  ((yaml-mode-hook yaml-ts-mode-hook) . electric-pair-local-mode)
  ((yaml-mode-hook yaml-ts-mode-hook) .
   (lambda ()
     (toggle-truncate-lines +1)
     (unless (gearp! :editing yaml -display-line-numbers)
       (display-line-numbers-mode +1))))
  :config
  (leaf eglot
    :doc "Language Server Protocol support for yaml-mode"
    :when (gearp! :editing yaml lsp)
    :doctor
    ("yaml-language-server" . "a Language Server for YAML files")
    :hook ((yaml-mode-hook yaml-ts-mode-hook) . eglot-ensure)
    :config
    (add-to-list 'eglot-server-programs '(yaml-mode . ("yaml-language-server" "--stdio")))
    (add-to-list 'eglot-server-programs '(yaml-ts-mode . ("yaml-language-server" "--stdio")))))
