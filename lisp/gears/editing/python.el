(leaf python-mode
  :doc "major mode for the Python Programming Language"
  :ensure (python-mode :ref "5aaf8b386aa694429d997c6fd49772b0b359e514")
  :when (gearp! :editing python)
  :config
  (leaf eglot
    :doc "Language Server Protocol support for python"
    :when (gearp! :editing python lsp)
    :hook (python-mode-hook . eglot-ensure)
    :config
    (add-to-list 'eglot-server-programs
		 `(python-mode . ,(eglot-alternatives
				   '("pylsp"
				     "pyls"
				     ("basedpyright-langserver" "--stdio")
				     ("pyright-langserver" "--stdio")
				     ("pyrefly" "lsp")
				     "jedi-language-server"
				     ("ruff" "server")))))
    :doctor
    ("pylsp" . "python implementation of the Language Server Protocol")
    ("pyls"  . "an implementation of the Language Server Protocol for Python")
    ("basedpyright-langserver" . "Basedpyright is a fork of pyright with various type checking improvements, and pylance features built into the language server")
    ("pyright-langserver" . "a full-featured, standards-based static type checker for Python")
    ("pyrefly" . "a fast type checker and language server for Python with powerful IDE features")
    ("jedi-language-server" . "a language server for the latest version(s) of Jedi")
    ("ruff" . "an extremely fast Python linter, code formatter (and LSP server), written in Rust"))

  (unless (gearp! :editing python -treesit)
    (add-to-list 'treesit-auto-langs 'python)
    (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
    (setq python-ts-mode-hook python-mode-hook)))
