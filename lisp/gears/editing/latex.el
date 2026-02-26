(leaf auctex
  :doc "an extensible package for writing and formatting TeX files"
  :when (gearp! :editing latex)
  :ensure (auctex :ref "f0c4b1dcc9e5987dce43b1e43f530351157ff577" :host github :repo "emacs-straight/auctex")
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
