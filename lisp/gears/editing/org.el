(leaf org
  :doc "your life in plain text -- notes, todos, spreadsheets, and accidentally a whole operating system"
  :when (gearp! :editing org)
  :mode "\\.org\\'"
  :ensure (org :ref "8b15a0d0b48a0e3ce09be0d208d74a01743cbbe0" :host github :repo "emacs-straight/org-mode")
  :config
  (when (gearp! :editing org display-line-numbers)
    (add-hook 'org-mode-hook #'display-line-numbers-mode)))
