(progn
  (elpaca-wait)
  (save-default-bg-fg-colors)
  (unless (gearp! :ui -treesit)
    (let ((treesit-auto-install t))
      (treesit-auto-install-all)))
  (kill-emacs 0))
