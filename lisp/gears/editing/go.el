;; Declare tree-sitter languages needed by this gear
(when (and (gearp! :editing go)
           (not (gearp! :editing go -treesit)))
  (backpack-treesit-langs! go gomod)

  (add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode)))

(leaf go-mode
  :doc "Support for Go programming language in Emacs"
  :when (gearp! :editing go)
  :ensure (go-mode :ref "0ed3c5227e7f622589f1411b4939c3ee34711ebd")
  :hook
  ((go-mode-hook go-ts-mode-hook) . electric-pair-local-mode)
  ((go-mode-hook go-ts-mode-hook) .
   (lambda ()
     (toggle-truncate-lines +1)
     (unless (gearp! :editing go -display-line-numbers)
       (display-line-numbers-mode +1))))
  :config
  (leaf eglot
    :doc "Language Server Protocol support for go-mode"
    :when (gearp! :editing go lsp)
    :doctor ("gopls" . "the official language server protocol (lsp) implementation provided by the Go team")
    :hook ((go-mode-hook go-ts-mode-hook) . eglot-ensure)
    :config
    (add-to-list 'eglot-server-programs '(go-mode . ("gopls" "serve")))
    (add-to-list 'eglot-server-programs '(go-ts-mode . ("gopls" "serve"))))

  (leaf go-impl
    :doc "generates method stubs for implementing an interface"
    :when (gearp! :editing go impl)
    :ensure (go-impl :ref "1eebba6ccd02d11a5a82ad4540a8d562797bc3b3")
    :doctor ("impl" . "command that generates method stubs for implementing an interface"))

  (leaf go-rename
    :doc "Integration of the 'gorename' tool into Emacs"
    :when (gearp! :editing go rename)
    :doctor ("gorename" . "command that performs precise type-safe renaming of identifiers in Go source code")))
