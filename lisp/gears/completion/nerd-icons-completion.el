(require 'backpack-pouch)

(leaf nerd-icons-completion
  :doc "use nerd-icons on completion candidates"
  :ensure (nerd-icons-completion :ref "63a6b3f1eb98bb381c86a1658ac401c8967079b8")
  :unless (or (gearp! :completion -marginalia) ;; unless marginalia was turn off
	      (gearp! :completion marginalia -nerd)) ;; or the user doesn't want marginalia with nerd font icons
  :fonts ("Symbols Nerd Font" . "Recommended by nerd-icons package")
  :custom
  (nerd-icons-font-family . "Symbols Nerd Font Mono")
  :config
  (nerd-icons-completion-mode)
  (with-eval-after-load 'marginalia
    (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)))
