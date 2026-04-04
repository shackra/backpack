;; Declare tree-sitter languages needed by this gear
(when (and (gearp! :editing markdown)
           (not (gearp! :editing markdown -treesit)))
  (backpack-treesit-langs! markdown)

  ;; replace the original recipe, as the new split_parser branch is
  ;; the default and there is no src folder in the root of the project
  (with-eval-after-load 'treesit-auto
    (add-to-list 'treesit-auto-recipe-list
		 (make-treesit-auto-recipe
		  :lang 'markdown
		  :ts-mode 'markdown-ts-mode
		  :remap '(markdown-mode gfm-mode)
		  :url "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
		  :source-dir "tree-sitter-markdown/src"
		  :requires '((markdown_inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
                                               "split_parser"
                                               "tree-sitter-markdown-inline/src"))))))

(leaf markdown-mode
  :doc "the markup language everyone thinks they know until they hit nested lists"
  :when (gearp! :editing markdown)
  :ensure (markdown-mode :ref "9de2df5a9f2f864c82ec112d3369154767a2bb49")
  :mode ("\\.md\\'" "\\.markdown\\'" "\\.mkd\\'")
  :hook
  ((markdown-mode-hook markdown-ts-mode-hook) . visual-line-mode)
  :config
  (when (gearp! :editing markdown display-line-numbers)
    (add-hook 'markdown-mode-hook #'display-line-numbers-mode)
    (add-hook 'markdown-ts-mode-hook #'display-line-numbers-mode)))

(leaf markdown-ts-mode
  :doc "tree-sitter support for Markdown"
  :ensure (markdown-ts-mode :ref "2f1ee8b94cdf53cebc31ae08ecfbba846193d5e1")
  :unless (gearp! :editing markdown -treesit)
  :after markdown-mode)
