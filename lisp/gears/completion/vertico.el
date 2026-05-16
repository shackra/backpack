(leaf vertico
  :doc "provides a performant and minimalistic vertical completion UI based on the default completion system"
  :unless (gearp! :completion -vertico)
  :ensure (vertico :ref "488685badfdf49bb750a213f228bdba8e113c7c8")
  :leaf-defer nil
  :bind
  (:vertico-map ("?"		.	#'minibuffer-completion-help)
		("M-RET"	.	#'minibuffer-force-complete-and-exit)
		("M-TAB"	.	#'minibuffer-complete))
  :custom
  (vertico-cycle			.	t)
  (context-menu-mode			.	t)
  (read-extended-command-predicate	.	#'command-completion-default-include-p)
  (minibuffer-prompt-properties		.       '(read-only t cursor-intangible t face minibuffer-prompt))
  :config
  (leaf vertico-directory
    :doc "provides Ido-like directory navigation commands"
    :bind (:vertico-map
	   ("RET"	.	#'vertico-directory-enter)
	   ("DEL"	.	#'vertico-directory-delete-char)
	   ("M-DEL"	.	#'vertico-directory-delete-word))
    :hook (rfn-eshadow-update-overlay-hook . #'vertico-directory-tidy)
    :leaf-defer nil)
  (vertico-mode))

(leaf vertico-flat
  :doc "a flat, horizontal display"
  :after vertico
  :when (gearp! :completion vertico flat)
  :config
  (vertico-flat-mode))

(leaf vertico-quick
  :doc "Avy-style quick selection with labeled candidates.
Credit: backpack-vertico-quick-embark adapted from chiply/.zetta.d"
  :after vertico
  :when (gearp! :completion vertico quick)
  :preface
  (defun backpack-vertico-quick-embark (&optional arg)
    "Embark on candidate using quick keys."
    (interactive)
    (when (vertico-quick-jump)
      (embark-act arg)))
  :bind (:vertico-map
	 ("C-'"  . vertico-quick-exit)
	 ("C-\"" . backpack-vertico-quick-embark)))

(leaf vertico-repeat
  :doc "resume last completion session"
  :after vertico
  :when (gearp! :completion vertico repeat)
  :bind ("s-V" . vertico-repeat)
  :hook (minibuffer-setup-hook . vertico-repeat-save))

(leaf vertico-suspend
  :doc "suspend and resume completion sessions"
  :after vertico
  :when (gearp! :completion vertico suspend)
  :bind
  ("M-S"        . vertico-suspend)
  (:vertico-map ("M-S" . vertico-suspend)))
