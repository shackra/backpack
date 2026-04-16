;; Declare tree-sitter languages needed by this gear
(when (and (gearp! :editing rust)
           (not (gearp! :editing rust -treesit)))
  (backpack-treesit-recipe! rust
    :ts-mode 'rust-ts-mode
    :remap 'rust-mode
    :url "https://github.com/tree-sitter/tree-sitter-rust"
    :ext "\\.rs\\'"
    :versions ((:until-emacs "29.4" :revision "9c84af007b0f144954adb26b3f336495cbb320a7"))))

(leaf rust-mode
  :doc "the borrow checker said no, and it was right"
  :ensure (rust-mode :ref "f68ddca5c22b94a2de7c9ce20d629cd78d60b269")
  :when (gearp! :editing rust)
  :hook
  ((rust-mode-hook rust-ts-mode-hook) . electric-pair-local-mode)
  ((rust-mode-hook rust-ts-mode-hook) .
   (lambda ()
     (toggle-truncate-lines +1)
     (unless (gearp! :editing rust -display-line-numbers)
       (display-line-numbers-mode +1))))
  :config
  (leaf cargo-mode
    :doc "Cargo process support for Emacs"
    :when (gearp! :editing rust cargo)
    :ensure (cargo-mode :ref "33528954218f8957a26f3fef506c3537823d569d")
    :doctor ("cargo" . "the Rust package manager")
    :custom (compilation-scroll-output . t)
    :hook ((rust-mode-hook rust-ts-mode-hook) . cargo-minor-mode)))

(leaf eglot
  :doc "Language Server Protocol support for rust-mode"
  :when (and (gearp! :editing rust) (gearp! :editing rust lsp))
  :doctor
  ("rust-analyzer" . "an implementation of Language Server Protocol for the Rust programming language")
  :hook ((rust-mode-hook rust-ts-mode-hook) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(rust-mode . ("rust-analyzer")))
  (add-to-list 'eglot-server-programs '(rust-ts-mode . ("rust-analyzer"))))

(leaf ob-rust
  :doc "Rust source blocks in org-mode"
  :when (and (gearp! :editing rust) (gearp! :editing org))
  :after org
  :ensure (ob-rust :ref "be059d231fafeb24a658db212a55ccdc55c0c500")
  :doctor ("rustc" . "the Rust compiler")
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages (append org-babel-load-languages '((rust . t)))))
