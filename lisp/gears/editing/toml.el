;; Declare tree-sitter languages needed by this gear
(when (and (gearp! :editing toml)
           (not (gearp! :editing toml -treesit)))
  (backpack-treesit-langs! toml)

  (add-to-list 'major-mode-remap-alist '(toml-mode . toml-ts-mode)))

(leaf toml-mode
  :doc "A major-mode for editing TOML files"
  :ensure (toml-mode :ref "f6c61817b00f9c4a3cab1bae9c309e0fc45cdd06")
  :when (gearp! :editing toml)
  :hook
  ((toml-mode-hook toml-ts-mode-hook) . electric-pair-local-mode)
  ((toml-mode-hook toml-ts-mode-hook) .
   (lambda ()
     (toggle-truncate-lines +1)
     (unless (gearp! :editing toml -display-line-numbers)
       (display-line-numbers-mode +1))))
  :config
  (leaf eglot
    :doc "Language Server Protocol support for toml-mode"
    :when (gearp! :editing toml lsp)
    :doctor
    ("taplo" . "a TOML toolkit with LSP, formatter, and validator")
    :hook ((toml-mode-hook toml-ts-mode-hook) . eglot-ensure)
    :config
    (add-to-list 'eglot-server-programs '(toml-mode . ("taplo" "lsp" "stdio")))
    (add-to-list 'eglot-server-programs '(toml-ts-mode . ("taplo" "lsp" "stdio")))))
