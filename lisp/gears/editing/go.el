;; Declare tree-sitter languages needed by this gear
(when (and (gearp! :editing go)
           (not (gearp! :editing go -treesit)))
  (backpack-treesit-recipe! go
    :ts-mode 'go-ts-mode
    :remap 'go-mode
    :url "https://github.com/tree-sitter/tree-sitter-go"
    :ext "\\.go\\'"
    :versions ((:until-emacs "29.4" :revision "7ee8d928db5202f6831a78f8112fd693bf69f98b")
               (:until-emacs "30.2" :revision "12fe553fdaaa7449f764bc876fd777704d4fb752")))
  (backpack-treesit-recipe! gomod
    :ts-mode 'go-mod-ts-mode
    :remap 'go-mod-mode
    :url "https://github.com/camdencheek/tree-sitter-go-mod"
    :ext "go\\.mod\\'"
    :versions ((:until-emacs "29.4" :revision "bbe2fe3be4b87e06a613e685250f473d2267f430"))))

(leaf go-mode
  :doc "if err != nil { return nil, err } -- the language"
  :when (gearp! :editing go)
  :ensure (go-mode :ref "0ed3c5227e7f622589f1411b4939c3ee34711ebd")
  :hook
  ((go-mode-hook go-ts-mode-hook) . electric-pair-local-mode)
  ((go-mode-hook go-ts-mode-hook) .
   (lambda ()
     (toggle-truncate-lines +1)
     (unless (gearp! :editing go -display-line-numbers)
       (display-line-numbers-mode +1)))))

(leaf go-impl
  :doc "generates method stubs for implementing an interface"
  :when (gearp! :editing go impl)
  :ensure (go-impl :ref "1eebba6ccd02d11a5a82ad4540a8d562797bc3b3")
  :doctor ("impl" . "command that generates method stubs for implementing an interface"))

(leaf go-rename
  :doc "Integration of the 'gorename' tool into Emacs"
  :when (gearp! :editing go rename)
  :doctor ("gorename" . "command that performs precise type-safe renaming of identifiers in Go source code"))

(leaf eglot
  :doc "Language Server Protocol support for go-mode"
  :when (and (gearp! :editing go) (gearp! :editing go lsp))
  :doctor ("gopls" . "the official language server protocol (lsp) implementation provided by the Go team")
  :hook ((go-mode-hook go-ts-mode-hook) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(go-mode . ("gopls" "serve")))
  (add-to-list 'eglot-server-programs '(go-ts-mode . ("gopls" "serve"))))
