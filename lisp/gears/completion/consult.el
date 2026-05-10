(leaf compat
  :doc "forward-compatibility library required by consult on Emacs < 31"
  :emacs< 31
  :unless (gearp! :completion -consult)
  :ensure (compat :ref "b5b48183689b536f72b1214106afeabc465da9d4"))

(leaf consult
  :doc "enhanced minibuffer commands: buffer switching, search, navigation, and more"
  :unless (gearp! :completion -consult)
  :ensure (consult :ref "c62e767869d640cf19c99401562f906cb20c9c55")
  :leaf-defer nil
  :bind
  ;; Replace built-in commands with consult equivalents
  ([remap switch-to-buffer]			.	consult-buffer)
  ([remap switch-to-buffer-other-window]	.	consult-buffer-other-window)
  ([remap switch-to-buffer-other-frame]		.	consult-buffer-other-frame)
  ([remap goto-line]				.	consult-goto-line)
  ([remap yank-pop]				.	consult-yank-pop)
  ([remap bookmark-jump]			.	consult-bookmark)
  ([remap project-switch-to-buffer]		.	consult-project-buffer)
  ([remap imenu]				.	consult-imenu)
  ([remap repeat-complex-command]		.	consult-complex-command)
  ([remap project-find-regexp]			.	consult-ripgrep)
  ;; M-g (goto-map)
  ("M-g f"	.	consult-flymake)
  ("M-g o"	.	consult-outline)
  ("M-g m"	.	consult-mark)
  ("M-g M"	.	consult-global-mark)
  ("M-g I"	.	consult-imenu-multi)
  ;; M-s (search-map)
  ("M-s l"	.	consult-line)
  ("M-s L"	.	consult-line-multi)
  ("M-s r"	.	consult-ripgrep)
  ("M-s g"	.	consult-grep)
  ("M-s f"	.	consult-find)
  ("M-s k"	.	consult-keep-lines)
  ("M-s u"	.	consult-focus-lines)
  ;; Other
  ("C-c h"	.	consult-history)
  :custom
  (consult-narrow-key	.	"<")
  :config
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref))

(leaf consult-eglot
  :doc "workspace symbol search via eglot"
  :when (and (not (gearp! :completion -consult))
	     (gearp! :completion consult eglot)
	     (not (gearp! :completion -eglot)))
  :ensure (consult-eglot :ref "d8b444aac39edfc6473ffbd228df3e9119451b51")
  :after (consult eglot)
  :bind (:eglot-mode-map
	 ("C-c l s"	.	consult-eglot-symbols)))

(leaf consult-dir
  :doc "quick directory switching in minibuffer"
  :when (and (not (gearp! :completion -consult))
	     (gearp! :completion consult dir))
  :ensure (consult-dir :ref "1497b46d6f48da2d884296a1297e5ace1e050eb5")
  :after consult
  :bind
  ("C-x C-d"	.	consult-dir)
  (:vertico-map
   ("C-x C-d"	.	consult-dir)
   ("C-x C-j"	.	consult-dir-jump-file)))

(leaf consult-project-extra
  :doc "enhanced project file and buffer finding"
  :when (and (not (gearp! :completion -consult))
	     (gearp! :completion consult project-extra))
  :ensure (consult-project-extra :ref "52c453b7f85dea90cc53ff8fba750795606415df")
  :after consult
  :bind
  ([remap project-find-file]	.	consult-project-extra-find))

(leaf consult-todo
  :doc "jump to TODO/FIXME/HACK comments in project"
  :when (and (not (gearp! :completion -consult))
	     (gearp! :completion consult todo))
  :ensure (consult-todo :ref "f9ba063a6714cb95ddbd886786ada93771f3c140")
  :after consult
  :bind
  (:project-prefix-map
   ("t"	.	consult-todo)
   ("T"	.	consult-todo-all)))

(leaf consult-org-roam
  :doc "org-roam node search via consult"
  :when (and (not (gearp! :completion -consult))
	     (gearp! :completion consult org-roam)
	     (gearp! :editing org roam))
  :ensure (consult-org-roam :ref "781d9c1cfee8631bc125fa45bab92de320d3941e")
  :after (consult org-roam)
  :custom
  (consult-org-roam-grep-func	.	#'consult-ripgrep)
  :config
  (consult-org-roam-mode 1))
