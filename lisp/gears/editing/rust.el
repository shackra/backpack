;; Declare tree-sitter languages needed by this gear
(when (and (gearp! :editing rust)
           (not (gearp! :editing rust -treesit)))
  (backpack-treesit-langs! rust)

  (add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode)))

(leaf rust-mode
  :doc "A major-mode for editing Rust source code"
  :ensure (rust-mode :ref "d00d83d3a207a5b7c2994392b2781f627e3159ce")
  :when (gearp! :editing rust)
  :hook
  ((rust-mode-hook rust-ts-mode-hook) . electric-pair-local-mode)
  ((rust-mode-hook rust-ts-mode-hook) .
   (lambda ()
     (toggle-truncate-lines +1)
     (unless (gearp! :editing rust -display-line-numbers)
       (display-line-numbers-mode +1))))
  :config
  (leaf eglot
    :doc "Language Server Protocol support for rust-mode"
    :when (gearp! :editing rust lsp)
    :doctor
    ("rust-analyzer" . "an implementation of Language Server Protocol for the Rust programming language")
    :hook ((rust-mode-hook rust-ts-mode-hook) . eglot-ensure)
    :config
    (add-to-list 'eglot-server-programs '(rust-mode . ("rust-analyzer")))
    (add-to-list 'eglot-server-programs '(rust-ts-mode . ("rust-analyzer"))))

  (leaf cargo
    :doc "Cargo process support for Emacs"
    :when (gearp! :editing rust cargo)
    :ensure (cargo :ref "5f5a7b5bd4bf671d3a00c0523e6946c9a6f0a509")
    :doctor ("cargo" . "the Rust package manager")
    :hook ((rust-mode-hook rust-ts-mode-hook) . cargo-minor-mode)))

(leaf ob-rust
  :doc "Rust source blocks in org-mode"
  :when (and (gearp! :editing rust) (gearp! :editing org))
  :after org
  :ensure (ob-rust :ref "d0fab5ec33a31413643124c744da10bfbed4ef01")
  :doctor ("rustc" . "the Rust compiler")
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages (append org-babel-load-languages '((rust . t)))))
