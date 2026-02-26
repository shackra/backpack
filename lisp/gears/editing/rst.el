(leaf rst
  :doc "a major mode for editing reStructuredText documents"
  :tag "builtin"
  :when (gearp! :editing rst)
  :mode ("\\.rst\\'" "\\.rest\\'")
  :hook
  (rst-mode-hook . visual-line-mode)
  :config
  (when (gearp! :editing rst display-line-numbers)
    (add-hook 'rst-mode-hook #'display-line-numbers-mode)))
