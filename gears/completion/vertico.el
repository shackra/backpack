(require 'backpack-pouch)

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
