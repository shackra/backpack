(leaf asm-mode
  :doc "closer to the metal than you ever wanted to be"
  :when (gearp! :editing assembler)
  :hook
  (asm-mode-hook . electric-pair-local-mode)
  (asm-mode-hook .
		 (lambda ()
		   (toggle-truncate-lines +1)
		   (unless (gearp! :editing assembler -display-line-numbers)
		     (display-line-numbers-mode +1)))))

(leaf nasm-mode
  :doc "NASM x86 assembly major mode"
  :when (gearp! :editing assembler nasm)
  :ensure (nasm-mode :ref "4e670f6dededab858251670aa5459c950f78d867")
  :hook (nasm-mode-hook . electric-pair-local-mode)
  (nasm-mode-hook .
		  (lambda ()
		    (toggle-truncate-lines +1)
		    (unless (gearp! :editing assembler -display-line-numbers)
		      (display-line-numbers-mode +1)))))

(leaf eglot
  :doc "Language Server Protocol support for assembly"
  :when (and (gearp! :editing assembler) (gearp! :editing assembler lsp))
  :doctor ("asm-lsp" . ("a language server for assembly providing hover, completion, diagnostics, and go-to-definition" optional))
  :hook ((asm-mode-hook nasm-mode-hook) . eglot-ensure)
  :config
  ;; LSP protocol extensions note: asm-lsp uses standard LSP features.
  ;; It reads .asm-lsp.toml config files for per-project settings.
  ;; No non-standard protocol extensions documented.
  (add-to-list 'eglot-server-programs '(asm-mode . ("asm-lsp")))
  (add-to-list 'eglot-server-programs '(nasm-mode . ("asm-lsp"))))
