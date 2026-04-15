(leaf org
  :doc "your life in plain text -- notes, todos, spreadsheets, and accidentally a whole operating system"
  :when (gearp! :editing org)
  :mode "\\.org\\'"
  :ensure (org :ref "8b15a0d0b48a0e3ce09be0d208d74a01743cbbe0" :host github :repo "emacs-straight/org-mode")
  :hook
  (org-mode-hook . (lambda ()
		     (when (gearp! :editing org display-line-numbers)
		       (display-line-numbers-mode))))
  (org-mode-hook . visual-line-mode)
  :custom
  (org-startup-with-latex-preview . t))

(leaf mixed-pitch
  :doc "a minor mode that enables mixing fixed-pitch (also known as fixed-width or monospace) and variable-pitch (AKA “proportional”) fonts"
  :url "https://gitlab.com/jabranham/mixed-pitch"
  :unless (gearp! :editing org -mixed-pitch)
  :after org
  :ensure (mixed-pitch :ref "519e05f74825abf04b7d2e0e38ec040d013a125a" :host gitlab :repo "jabranham/mixed-pitch")
  :hook (org-mode-hook . mixed-pitch-mode))

(leaf olivetti
  :doc "a simple Emacs minor mode for a nice writing environment"
  :url "https://github.com/rnkn/olivetti"
  :unless (gearp! :editing org -centered)
  :after org
  :ensure (olivetti :ref "845eb7a95a3ca3325f1120c654d761b91683f598" :host github :repo "rnkn/olivetti")
  :hook (org-mode-hook . olivetti-mode))

(leaf org-modern
  :doc "a modern style for your Org buffers using font locking and text properties"
  :unless (gearp! :editing -modern)
  :ensure (org-modern :ref "713beb72aed4db43f8a10feed72136e931eb674a" :host github :repo "minad/org-modern")
  :custom
  (org-adapt-indentation		.	t)
  (org-agenda-tags-column		.	0)
  (org-auto-align-tags			.	nil)
  (org-catch-invisible-edits		.	'show-and-error)
  (org-edit-src-content-indentation	.	0)
  (org-ellipsis				.	"…")
  (org-hide-emphasis-markers		.	t)
  (org-insert-heading-respect-content	.	t)
  (org-pretty-entities			.	t)
  (org-special-ctrl-a/e			.	t)
  (org-src-fontify-natively		.	t)
  (org-src-tab-acts-natively		.	t)
  (org-tags-column			.	0)
  :global-minor-mode global-org-modern-mode
  :hook
  (org-mode-hook . (lambda () (setq line-spacing 0.2)))
  (org-mode-hook . variable-pitch-mode)
  :config
  ;; Resize Org headings
  (dolist (face '((org-level-1 . 1.25)
                  (org-level-2 . 1.2)
                  (org-level-3 . 1.2)
                  (org-level-4 . 1.2)
                  (org-level-5 . 1.2)
                  (org-level-6 . 1.2)
                  (org-level-7 . 1.2)
                  (org-level-8 . 1.2)))
    (set-face-attribute (car face) nil :weight 'bold :height (cdr face)))

  ;; increase the size of LaTeX-previews
  (plist-put org-format-latex-options :scale 2)

  ;; Make the document title a bit bigger
  (set-face-attribute 'org-document-title nil   :weight 'bold :height 1.6)
  (set-face-attribute 'org-block nil            :foreground 'unspecified :inherit 'fixed-pitch :height 0.85)
  (set-face-attribute 'org-code nil             :inherit '(shadow fixed-pitch) :height 0.85)
  (set-face-attribute 'org-verbatim nil         :inherit '(shadow fixed-pitch) :height 0.85)
  (set-face-attribute 'org-special-keyword nil  :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil        :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil         :inherit 'fixed-pitch))

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
  (:around org-roam-db-autosync-mode
	   (lambda (orig-fun arg &rest args)
	     (if (and (or (null arg)
			  (eq arg t)
			  (and (numberp arg) (> arg 0)))
		      (not (backpack--org-roam-check-directory)))
		 (user-error "`org-roam-directory' not set or does not exist, please set it in your private configuration")
	       (apply orig-fun arg args))))

  (:before org-roam-db-sync
	   (lambda (&rest _)
	     (if (not (backpack--org-roam-check-directory))
		 (user-error "`org-roam-directory' not set or does not exist, please set it in your private configuration"))))

  :hook (after-init-hook .  org-roam-db-autosync-mode))

(leaf org-noter
  :doc "a synchronized, Org-mode, document annotator"
  :url "https://github.com/weirdNox/org-noter"
  :when (gearp! :editing org noter)
  :after org
  :ensure (org-noter :ref "9ead81d42dd4dd5074782d239b2efddf9b8b7b3d" :host github :repo "weirdNox/org-noter"))
