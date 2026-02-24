;; Declare tree-sitter languages needed by this gear
(when (and (gearp! :editing python)
           (not (gearp! :editing python -treesit)))
  (backpack-treesit-langs! python)

  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode)))

(leaf python-mode
  :doc "major mode for the Python Programming Language"
  :ensure (python-mode :ref "5aaf8b386aa694429d997c6fd49772b0b359e514")
  :when (gearp! :editing python)
  :hook
  (python-mode-hook . electric-pair-local-mode)
  (python-mode-hook .
		    (lambda ()
		      (toggle-truncate-lines 1)
		      (unless (gearp! :editing python -display-line-numbers)
			(display-line-numbers-mode +1))))
  :config
  (leaf eglot
    :doc "Language Server Protocol support for python"
    :when (gearp! :editing python lsp)
    :hook
    (python-mode-hook . eglot-ensure)
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
    ("ruff" . "an extremely fast Python linter, code formatter (and LSP server), written in Rust")))

(leaf ob-python
  :doc "Python source blocks in org-mode"
  :when (and (gearp! :editing python) (gearp! :editing org))
  :after org
  :custom
  (org-babel-python-command . "python3")
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages (append org-babel-load-languages '((python . t)))))

(leaf python-ts-mode
  :doc "tree-sitter support for Python"
  :after python
  :unless (gearp! :editing python -treesit)
  :config
  (setq python-ts-mode-hook python-mode-hook))
