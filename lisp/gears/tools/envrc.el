(leaf envrc
  :doc "Emacs support for direnv which operates buffer-locally"
  :when (gearp! :tools direnv)
  :ensure (envrc :ref "de1ae6e538764f74659f358b04af0d84fa0fef42")
  :global-minor-mode envrc-global-mode
  :bind-keymap
  ("C-c e" . envrc-command-map))
