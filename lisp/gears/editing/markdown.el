;; Declare tree-sitter languages needed by this gear
(when (and (gearp! :editing markdown)
           (not (gearp! :editing markdown -treesit)))
  (backpack-treesit-langs! markdown))

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
