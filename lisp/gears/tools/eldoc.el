(leaf eldoc
  :doc "Originally Emacs Lisp Documentation, now it offers documentation lookup for all major modes"
  :unless (gearp! :tools -eldoc)
  :global-minor-mode global-eldoc-mode)

(leaf eldoc-box
  :when (gearp! :tools eldoc box)
  :ensure (eldoc-box :ref "fead2cef661790417267e5498d4d14806e020f99")
  :custom
  (eldoc-box-hover-display-frame-above-point . t)
  :hook
  (eldoc-mode-hook . eldoc-box-hover-at-point-mode))
