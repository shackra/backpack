(leaf auctex
  :doc "an extensible package for writing and formatting TeX files"
  :when (gearp! :editing latex)
  :ensure (auctex :ref "9779fa472e50891666e23dde541e01c3cfc852ec")
  :mode ("\\.tex\\'" . LaTeX-mode)
  :hook
  (LaTeX-mode-hook . visual-line-mode)
  :custom
  (TeX-auto-save . t)
  (TeX-parse-self . t)
  (TeX-master . nil)
  :config
  (when (gearp! :editing latex display-line-numbers)
    (add-hook 'LaTeX-mode-hook #'display-line-numbers-mode))

  (leaf eglot
    :when (gearp! :editing latex lsp)
    :doc "Language Server Protocol support for LaTeX"
    :doctor
    ("texlab" . "an implementation of the Language Server Protocol for LaTeX")
    :hook
    (LaTeX-mode-hook . eglot-ensure)))
