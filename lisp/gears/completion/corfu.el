;; TODO(shackra): handle when Emacs is ran in a terminal
(leaf corfu
  :doc "enhances in-buffer completion with a small completion popup"
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
  :custom
  (tab-always-indent . 'complete)
  (text-mode-ispell-word-completion . nil)
  (read-extended-command-predicate . #'command-completion-default-include-p)
  :hook
  (corfu-mode-hook . corfu-popupinfo-mode)
  :global-minor-mode global-corfu-mode)
