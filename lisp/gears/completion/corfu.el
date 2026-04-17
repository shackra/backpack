(leaf corfu
  :doc "enhances in-buffer completion with a small completion popup.
Flags: auto-for-prog (auto in prog-mode), auto-for-text (auto in text-mode),
auto-idle (zero-prefix auto when LSP active, 1s delay),
auto-idle-slow (with auto-idle: use 2s delay instead of 1s).
On Emacs < 31 in terminal, uses corfu-terminal (overlay-based popup)."
  :when (gearp! :completion corfu)
  :ensure (corfu :ref "76ee8f4e57d4cfb0deb3988cde199e6028cfbe7e")
  :init
  (when (gearp! :completion corfu auto-for-prog)
    (add-hook 'prog-mode-hook (lambda () (setq-local corfu-auto t))))

  (when (gearp! :completion corfu auto-for-text)
    (add-hook 'text-mode-hook (lambda () (setq-local corfu-auto t))))

  (when (or (gearp! :completion corfu auto-idle) (gearp! :completion corfu auto-idle-slow))
    (add-hook 'eglot-managed-mode-hook
              (lambda ()
                (setq-local corfu-auto t)
                (setq-local corfu-auto-prefix 0)
                (setq-local corfu-auto-delay
                            (if (gearp! :completion corfu auto-idle-slow)
                                2.0
                              1.0))
                (setq-local corfu-auto-commands
                            (append corfu-auto-commands '(newline newline-and-indent))))))
  :custom
  (tab-always-indent			.	'complete)
  (text-mode-ispell-word-completion	.	nil)
  (read-extended-command-predicate	.	#'command-completion-default-include-p)
  :hook
  (corfu-mode-hook . corfu-popupinfo-mode)
  :global-minor-mode global-corfu-mode)

(leaf popon
  :doc "Overlay-based popup library for corfu-terminal"
  :emacs< 31
  :when (gearp! :completion corfu)
  :ensure (popon :ref "bf8174cb7e6e8fe0fe91afe6b01b6562c4dc39da"))

(leaf corfu-terminal
  :doc "Corfu popup on terminal. Auto-enabled when Emacs < 31 runs in a TTY."
  :emacs< 31
  :when (gearp! :completion corfu)
  :ensure (corfu-terminal :ref "501548c3d51f926c687e8cd838c5865ec45d03cc")
  :after corfu
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))
