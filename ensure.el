(progn
  (elpaca-wait)
  (save-default-bg-fg-colors)
  (unless (gearp! :ui -treesit)
    (treesit-auto-install-all))
  (kill-emacs 0))
