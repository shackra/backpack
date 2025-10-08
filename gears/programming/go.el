(require 'backpack-pouch)

(leaf go-mode
  :doc "Support for Go programming language in Emacs"
  :when (gearp! :programming go)
  :ensure (go-mode :ref "0ed3c5227e7f622589f1411b4939c3ee34711ebd")
  :config
  (leaf eglot
    :doc "Language Server Protocol support for go-mode"
    :when (gearp! :programming go lsp)
    :doctor ("gopls" . "the official language server protocol (lsp) implementation provided by the Go team")
    :hook (go-mode-hook . eglot-ensure)
    :config
    (add-to-list 'eglot-server-programs '(go-mode . ("gopls" "serve"))))

  (leaf go-impl
    :doc "generates method stubs for implementing an interface"
    :when (gearp! :programming go impl)
    :ensure (go-impl :ref "1eebba6ccd02d11a5a82ad4540a8d562797bc3b3")
    :doctor ("impl" . "command that generates method stubs for implementing an interface"))

  (leaf go-rename
    :doc "Integration of the 'gorename' tool into Emacs"
    :when (gearp! :programming go rename)
    :doctor ("gorename" . "command that performs precise type-safe renaming of identifiers in Go source code"))

  (unless (gearp! :programming go -treesit)
    (add-to-list 'treesit-auto-langs 'go)
    (add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))
    (setq go-ts-mode-hook go-mode-hook)))
