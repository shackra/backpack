(leaf envrc
  :doc "Emacs support for direnv which operates buffer-locally"
  :when (gearp! :tools direnv)
  :ensure (envrc :ref "de1ae6e538764f74659f358b04af0d84fa0fef42")
  :global-minor-mode envrc-global-mode
  :bind-keymap
  ("C-c e" . envrc-command-map)
  :config
  ;; Prevent envrc from activating in user-emacs-directory.
  ;; Backpack itself is a devenv project, but we do not want its
  ;; .envrc to kick in when Emacs is installed there.
  (define-advice envrc-mode (:before-while (&optional arg) backpack--skip-emacs-dir)
    "Do not enable envrc-mode inside `user-emacs-directory'."
    (or (and (numberp arg) (< arg 1))  ; always allow disabling
        (not (file-in-directory-p default-directory user-emacs-directory)))))
