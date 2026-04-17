;; TODO(shackra): handle when Emacs is ran in a terminal
(leaf corfu
  :doc "enhances in-buffer completion with a small completion popup.
Flags: auto-for-prog (auto in prog-mode), auto-for-text (auto in text-mode),
auto-idle (zero-prefix auto when LSP active, 1s delay),
auto-idle-slow (with auto-idle: use 2s delay instead of 1s)"
  :when (gearp! :completion corfu)
  :ensure (corfu :ref "76ee8f4e57d4cfb0deb3988cde199e6028cfbe7e")
  :init
  (when (gearp! :completion corfu auto-for-prog)
    ;; activate auto-completion for programming major modes
    ;; WARNING: per corfu readme: this may be dangerous in non-trusted files or buffers
    (add-hook 'prog-mode-hook (lambda () (setq-local corfu-auto t))))

  (when (gearp! :completion corfu auto-for-text)
    ;; activate auto-completion for text major modes
    (add-hook 'text-mode-hook (lambda () (setq-local corfu-auto t))))

  (when (gearp! :completion corfu auto-idle)
    ;; Zero-prefix auto-completion when an LSP server is active.
    ;; Mirrors the "completions appear immediately" behaviour of vscode-yaml.
    ;; WARNING: corfu-auto-prefix 0 fires on every keystroke while an LSP is
    ;; running; use auto-idle-slow to reduce load on slower machines.
    (add-hook 'eglot-managed-mode-hook
              (lambda ()
                (setq-local corfu-auto t)
                (setq-local corfu-auto-prefix 0)
                (setq-local corfu-auto-delay
                            (if (gearp! :completion corfu auto-idle-slow)
                                2.0
                              1.0))
                (setq-local corfu-auto-commands
                            (append corfu-auto-commands '(newline newline-and-indent)))))
  :custom
  (tab-always-indent . 'complete)
  (text-mode-ispell-word-completion . nil)
  (read-extended-command-predicate . #'command-completion-default-include-p)
  :hook
  (corfu-mode-hook . corfu-popupinfo-mode)
  :global-minor-mode global-corfu-mode)
