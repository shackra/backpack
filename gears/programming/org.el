(require 'backpack-pouch)

(leaf org
  :doc "a major mode for keeping notes, authoring documents, computational notebooks, literate programming, maintaining to-do lists, planning projects, and more — in a fast and effective plain text system."
  :when (gearp! :programming org)
  :ensure (org :ref "8b15a0d0b48a0e3ce09be0d208d74a01743cbbe0" :host github :repo "emacs-straight/org-mode")
  :require t
  :config
  (unless (gearp! :programming org -treesit)
    (add-to-list 'treesit-auto-langs 'org)
    (add-to-list 'major-mode-remap-alist '(org-mode . org-ts-mode))
    (setq org-ts-mode-hook org-mode-hook)))
