(leaf markdown-mode
  :doc "a major mode for editing Markdown-formatted text"
  :when (gearp! :editing markdown)
  :ensure (markdown-mode :ref "9de2df5a9f2f864c82ec112d3369154767a2bb49")
  :mode ("\\.md\\'" "\\.markdown\\'" "\\.mkd\\'")
  :hook
  (markdown-mode-hook . visual-line-mode)
  :config
  (when (gearp! :editing markdown display-line-numbers)
    (add-hook 'markdown-mode-hook #'display-line-numbers-mode)))
