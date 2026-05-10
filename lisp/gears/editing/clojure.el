;; Declare tree-sitter languages needed by this gear
(when (and (gearp! :editing clojure)
           (not (gearp! :editing clojure -treesit)))
  (backpack-treesit-recipe! clojure
    :ts-mode 'clojure-ts-mode
    :remap 'clojure-mode
    :url "https://github.com/sogaiu/tree-sitter-clojure"
    :revision "unstable-20250526"
    :ext "\\\.clj[csx]?\\'")
  ;; clojure-ts-mode embeds markdown-inline (docstrings) and regex
  ;; (regex literal highlighting) grammars by default
  (backpack-treesit-recipe! markdown-inline
    :url "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
    :source-dir "tree-sitter-markdown-inline/src")
  (backpack-treesit-recipe! regex
    :url "https://github.com/tree-sitter/tree-sitter-regex"))

(leaf clojure-mode
  :doc "a monad is just a monoid in the category of endofunctors, what's the problem?"
  :ensure (clojure-mode :ref "c3b039ecf85e343edbc67c5856322654381dbc3e")
  :when (gearp! :editing clojure)
  :hook
  ((clojure-mode-hook clojure-ts-mode-hook) . electric-pair-local-mode)
  ((clojure-mode-hook clojure-ts-mode-hook) .
   (lambda ()
     (toggle-truncate-lines +1)
     (unless (gearp! :editing clojure -display-line-numbers)
       (display-line-numbers-mode +1)))))

(leaf clojure-ts-mode
  :doc "tree-sitter support for Clojure"
  :when (gearp! :editing clojure)
  :unless (gearp! :editing clojure -treesit)
  :ensure (clojure-ts-mode :ref "ba6de87b0acb5aa5483f6012611b30f6bf0414f3")
  :after clojure-mode
  :custom
  ;; Backpack handles grammar installation; disable clojure-ts-mode's
  ;; own installer to avoid redundant recompilation on file visit.
  (clojure-ts-ensure-grammars . nil))

(leaf cider
  :doc "the Clojure Interactive Development Environment that Rocks"
  :when (gearp! :editing clojure cider)
  :ensure (cider :ref "436645dbb4c60073577dc59e7596318c2501ac98"))

(leaf eglot
  :doc "Language Server Protocol support for Clojure"
  :when (and (gearp! :editing clojure) (gearp! :editing clojure lsp))
  :doctor ("clojure-lsp" . ("a language server for Clojure providing navigation, diagnostics, and refactoring" required))
  :hook ((clojure-mode-hook clojure-ts-mode-hook) . eglot-ensure)
  :config
  ;; LSP protocol extensions note: clojure-lsp supports non-standard extensions
  ;; for rename-file, clean-ns, stub-generation, extract-def, and move-coll.
  ;; Not yet implemented.
  (add-to-list 'eglot-server-programs '(clojure-mode . ("clojure-lsp")))
  (add-to-list 'eglot-server-programs '(clojure-ts-mode . ("clojure-lsp"))))

(leaf ob-clojure
  :doc "Clojure source blocks in org-mode"
  :when (and (gearp! :editing clojure) (gearp! :editing org))
  :after org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages (append org-babel-load-languages '((clojure . t)))))