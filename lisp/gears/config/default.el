(require 'backpack-pouch)
(require 'cl-lib)

(leaf default
  :when (gearp! :config default)
  :doc "Sane defaults for Emacs"
  :leaf-defer nil
  :bind
  ("C-x k" . kill-current-buffer)
  ("C-z" . nil)
  (:minibuffer-mode-map
   ("TAB" . minibuffer-complete))
  :setq
  (sentence-end-double-space			.	nil) ; fix archaic default
  (enable-recursive-minibuffers			.	t) ; use the minibuffer whilst in the minibuffer
  (completion-cycle-threshold			.	1) ; TAB cycles candidates
  (completions-detailed				.	t) ; show annotations
  (tab-always-indent				.	'complete) ; when I hit TAB, try to complete, otherwise, indent
  (completion-styles				.	'(basic initials substring)) ; different styles to match input to candidates
  (completion-auto-help				.	'always) ; open completion always; `lazy' another option
  (completions-max-height			.	10) ; this is arbitrary
  (completions-format				.	'one-column)
  (completions-group				.	t)
  (completion-auto-select			.	'second-tab) ; much more eager
  (switch-to-buffer-obey-display-actions	.	t) ; make switching buffers more consistent
  (x-underline-at-descent-line			.	nil) ; prettier underlines
  (indicate-buffer-boundaries			.	'left) ; Show buffer top and bottom in the margin
  (mouse-wheel-tilt-scroll			.	t) ; enable horizontal scrolling
  (mouse-wheel-flip-direction			.	t)
  (use-short-answers				.	t)
  :config
  (leaf autorevert
    :tag "builtin"
    :global-minor-mode global-auto-revert-mode
    :custom
    (auto-revert-avoid-polling	.	t)
    (auto-revert-interval	.	5)
    (auto-revert-check-vc-info	.	t))

  (leaf savehist
    :tag "builtin"
    :global-minor-mode savehist-mode)

  (line-number-mode t)		     ; show current line in modeline
  (column-number-mode t)	     ; show current column in modeline

  (when (gearp! :config default cua)
    (cua-mode))

  (when (gearp! :config default steady-cursor)
    (blink-cursor-mode -1))

  ;; enable the cursor in xterm when running Emacs
  ;; inside a terminal
  (unless (display-graphic-p)
    (xterm-mouse-mode 1))

  ;; highlight the current line unless the user
  ;; turn it off
  (unless (gearp! :config default -hl-line)
    (cl-loop for hook in '(prog-mode-hook text-mode-hook)
	     do
	     (add-hook hook #'hl-line-mode)))

  (when (gearp! :config default no-splash)
    (setq inhibit-splash-screen t))

  (when (gearp! :config default hide-tool-bar)
    (tool-bar-mode -1))

  (when (gearp! :config default hide-menu-bar)
    (menu-bar-mode -1))

  (unless (gearp! :config default -pixel-scroll)
    (pixel-scroll-precision-mode))

  (leaf dired
    :tag "builtin"
    :custom
    (dired-listing-switches . "-alh --group-directories-first")
    (dired-kill-when-opening-new-dired-buffer . t))

  ;; Under WSL, killed/copied text does not reach the Windows clipboard.
  ;; Pipe it through clip.exe so yanked text is available system-wide.
  ;; We write directly to the process stdin to avoid any shell quoting issues.
  (when backpack--system-wsl-p
    (setq interprogram-cut-function
          (lambda (text)
            (let ((process-connection-type nil))  ; use a pipe, not a pty
              (let ((proc (start-process "clip" nil "clip.exe")))
                (process-send-string proc text)
                (process-send-eof proc))))))

  (when (display-graphic-p)
    (context-menu-mode)))
