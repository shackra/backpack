;; Declare tree-sitter languages needed by this gear
(when (and (gearp! :editing nix)
           (not (gearp! :editing nix -treesit)))
  (backpack-treesit-langs! nix)

  (add-to-list 'major-mode-remap-alist '(nix-mode . nix-ts-mode)))

(leaf nix-mode
  :doc "an Emacs major mode for editing Nix expressions"
  :ensure (nix-mode :ref "719feb7868fb567ecfe5578f6119892c771ac5e5")
  :when (gearp! :editing nix)
  :hook
  ((nix-mode-hook nix-ts-mode-hook) . electric-pair-local-mode)
  ((nix-mode-hook nix-ts-mode-hook) .
   (lambda ()
     (toggle-truncate-lines +1)
     (unless (gearp! :editing nix -display-line-numbers)
       (display-line-numbers-mode +1)))))

(leaf eglot
  :doc "Language Server Protocol support for nix-mode"
  :when (and (gearp! :editing nix lsp) (gearp! :editing nix))
  :doctor
  ("nixd" . "a feature-rich nix language server interoperating with C++ nix")
  ("nil"  . "an incremental analysis assistant for writing in Nix")
  ("nixfmt" . "format Nix source code, used with your LSP server of choice")
  ("alejandra" . "the Uncompromising Nix Code Formatter, used with your LSP server of choice")
  :hook
  ((nix-mode-hook nix-ts-mode-hook) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
	       `(nix-mode . ,(eglot-alternatives '(("nixd")
						   ("nil" "--stdio")))))
  (add-to-list 'eglot-server-programs
		 `(nix-ts-mode . ,(eglot-alternatives '(("nixd")
						     ("nil" "--stdio"))))))

(leaf nix-ts-mode
  :doc "tree-sitter major mode for nix"
  :ensure (nix-ts-mode :ref "d769e53ccc0f40026fd11c7e23bf419c2caf4732")
  :unless (gearp! :editing nix -treesit)
  :after nix-mode)

(leaf ob-nix
  :doc "a simple org-babel language extension to evaluate nix expressions using `nix-instantiate`"
  :after org
  :ensure (ob-nix :ref "76d71b37fb031f25bd52ff9c98b29292ebe0424e")
  :when (and (gearp! :editing org) (gearp! :editing nix))
  :doctor
  ("nix-instantiate" . "instantiate store derivations from Nix expressions")
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages (append org-babel-load-languages '((nix . t)))))
