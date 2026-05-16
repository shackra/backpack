(leaf embark
  :doc "contextual actions on minibuffer candidates and beyond.
Enables embark-act, embark-act-all, embark-become, embark-export,
embark-select, and embark-dwim.
Credit: keybindings and indicator setup adapted from chiply/.zetta.d"
  :when (gearp! :completion embark)
  :ensure (embark :ref "ec5dd1475595277ef908567d0a18d32f1c40bc91")
  :leaf-defer nil
  :bind
  ("C-."   . embark-act)
  ("C-;"   . embark-dwim)
  ("C->"   . embark-act-all)
  ("C-h B" . embark-bindings)
  (:vertico-map
   ("C-."   . embark-act)
   ("C-;"   . embark-dwim)
   ("C->"   . embark-act-all)
   ("C-SPC" . embark-select))
  :custom
  (embark-help-key . "C-h")
  (embark-confirm-act-all . nil)
  :config
  (setq embark-indicators
	'(embark-verbose-indicator
	  embark-highlight-indicator
	  embark-minimal-indicator)))

(leaf embark-consult
  :doc "integration between embark and consult"
  :when (and (gearp! :completion embark)
	     (not (gearp! :completion -consult)))
  :ensure nil ;; ships with embark
  :after (embark consult))
