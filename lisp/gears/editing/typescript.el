;; Declare tree-sitter languages needed by this gear
(when (and (gearp! :editing typescript)
           (not (gearp! :editing typescript -treesit)))
  (backpack-treesit-recipe! typescript
    :ts-mode 'typescript-ts-mode
    :remap 'typescript-mode
    :url "https://github.com/tree-sitter/tree-sitter-typescript"
    :source-dir "typescript/src"
    :ext "\\.ts\\'"
    :versions ((:until-emacs "29.4" :revision "75b3874edb2dc714fb1fd77a32013d0f8699989f")))
  (backpack-treesit-recipe! tsx
    :ts-mode 'tsx-ts-mode
    :remap 'tsx-mode
    :url "https://github.com/tree-sitter/tree-sitter-typescript"
    :source-dir "tsx/src"
    :ext "\\.tsx\\'"
    :versions ((:until-emacs "29.4" :revision "75b3874edb2dc714fb1fd77a32013d0f8699989f"))))

(leaf typescript-ts-mode
  :doc "tree-sitter support for TypeScript"
  :when (gearp! :editing typescript)
  :unless (gearp! :editing typescript -treesit)
  :mode "\\.ts\\'"
  :hook
  ((typescript-ts-mode-hook tsx-ts-mode-hook) . electric-pair-local-mode)
  ((typescript-ts-mode-hook tsx-ts-mode-hook) .
   (lambda ()
     (toggle-truncate-lines +1)
     (unless (gearp! :editing typescript -display-line-numbers)
       (display-line-numbers-mode +1)))))

(leaf tsx-ts-mode
  :doc "tree-sitter support for TSX (React TypeScript)"
  :when (gearp! :editing typescript)
  :unless (gearp! :editing typescript -treesit)
  :after typescript-ts-mode)

(leaf eglot
  :doc "Language Server Protocol support for TypeScript"
  :when (and (gearp! :editing typescript) (gearp! :editing typescript lsp))
  :doctor ("typescript-language-server" . ("the LSP server for JavaScript and TypeScript" required))
  :hook ((typescript-ts-mode-hook tsx-ts-mode-hook) . eglot-ensure))

(leaf ob-typescript
  :doc "TypeScript source blocks in org-mode"
  :when (and (gearp! :editing typescript) (gearp! :editing org))
  :after org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages (append org-babel-load-languages '((typescript . t)))))
