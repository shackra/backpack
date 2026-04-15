(leaf org
  :doc "your life in plain text -- notes, todos, spreadsheets, and accidentally a whole operating system"
  :when (gearp! :editing org)
  :mode "\\.org\\'"
  :ensure (org :ref "8b15a0d0b48a0e3ce09be0d208d74a01743cbbe0" :host github :repo "emacs-straight/org-mode")
  :hook (org-mode-hook . (lambda ()
			   (when (gearp! :editing org display-line-numbers)
			     (display-line-numbers-mode)))))

(leaf org-modern
  :doc "a modern style for your Org buffers using font locking and text properties"
  :unless (gearp! :editing -modern)
  :ensure (org-modern :ref "713beb72aed4db43f8a10feed72136e931eb674a" :host github :repo "minad/org-modern")
  :custom
  (org-auto-align-tags			.	nil)
  (org-tags-column			.	0)
  (org-catch-invisible-edits		.	'show-and-error)
  (org-special-ctrl-a/e			.	t)
  (org-insert-heading-respect-content	.	t)
  (org-hide-emphasis-markers		.	t)
  (org-pretty-entities			.	t)
  (org-agenda-tags-column		.	0)
  (org-ellipsis				.	"…")
  :global-minor-mode global-org-modern-mode
  :hook
  (org-mode-hook . (lambda () (setq line-spacing 0.2))))

(leaf org-roam
  :doc "a plain-text knowledge management system. It brings some of Roam's more powerful features into the Org-mode ecosystem"
  :url "https://github.com/org-roam/org-roam"
  :when (gearp! :editing org roam)
  :after org
  :ensure (org-roam :ref "7cd906b6f8b18a21766228f074aff24586770934" :host github :repo "org-roam/org-roam")
  :setq
  (org-roam-node-display-template .  `,(concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  :bind
  ("C-c n l" . org-roam-buffer-toggle)
  ("C-c n f" . org-roam-node-find)
  ("C-c n g" . org-roam-graph)
  ("C-c n i" . org-roam-node-insert)
  ("C-c n c" . org-roam-capture)
  ;; for daily notes
  ("C-c n j" . org-roam-dailies-capture-today)
  :preface
  (defun backpack--org-roam-check-directory ()
    "Return t if `org-roam-directory' has a valid directory"
    (and (stringp org-roam-directory)
	 (not (string-empty-p (string-trim org-roam-directory)))
	 (file-directory-p (expand-file-name org-roam-directory))))
  :advice
  (:before org-roam-db-autosync-mode
	   (lambda (orig-func &rest arg)
	     (when arg
	       (if (not (backpack--org-roam-check-directory))
		   (user-error "`org-roam-directory' not set or does not exist")
		 (apply orig-func arg)))))

  (:before org-roam-db-sync
	   (lambda (orig-func &rest arg)
	     (if (not (backpack--org-roam-check-directory))
		 (user-error "`org-roam-directory' not set or does not exist")
	       (appy orig-func arg))))

  :global-minor-mode org-roam-db-autosync-mode)

(leaf org-noter
  :doc "a synchronized, Org-mode, document annotator"
  :url "https://github.com/weirdNox/org-noter"
  :when (gearp! :editing org noter)
  :after org
  :ensure (org-noter :ref "9ead81d42dd4dd5074782d239b2efddf9b8b7b3d" :host github :repo "weirdNox/org-noter"))
