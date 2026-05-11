;; Declare tree-sitter languages needed by this gear
(when (backpack-treesit-was-asked 'svelte "30.1")
  (backpack-treesit-recipe! svelte
    :ts-mode 'svelte-ts-mode
    :remap 'svelte-mode
    :url "https://github.com/tree-sitter-grammars/tree-sitter-svelte"
    :ext "\\.svelte\\'"
    :versions ((:until-abi 14 :revision "ae5199db47757f785e43a14b332118a5474de1a2"))))

(leaf svelte-mode
  :doc "a minor mode for Svelte component files"
  :when (gearp! :editing svelte)
  :ensure (svelte-mode :ref "ac8fba901dc790976f9893e338c8ad1241b897c6")
  :hook
  ((svelte-mode-hook svelte-ts-mode-hook) . electric-pair-local-mode)
  ((svelte-mode-hook svelte-ts-mode-hook) .
   (lambda ()
     (toggle-truncate-lines +1)
     (unless (gearp! :editing svelte -display-line-numbers)
       (display-line-numbers-mode +1)))))

(leaf svelte-ts-mode
  :doc "tree-sitter support for Svelte"
  :when (gearp! :editing svelte)
  :unless (gearp! :editing svelte -treesit)
  :emacs>= "30.1"
  :ensure (svelte-ts-mode :ref "d079050fc1ba70f8fba9e596638daa2ca96e0fdd" :host github :repo "leafOfTree/svelte-ts-mode")
  :after svelte-mode)

(leaf eglot
  :doc "Language Server Protocol support for Svelte"
  :when (and (gearp! :editing svelte) (gearp! :editing svelte lsp))
  :doctor ("svelteserver" . ("the Svelte language server, shipped with svelte-language-server" required))
  :hook ((svelte-mode-hook svelte-ts-mode-hook) . eglot-ensure)
  :config
  ;; LSP protocol extensions note: svelteserver uses standard LSP.
  ;; No non-standard protocol extensions documented.
  (add-to-list 'eglot-server-programs '(svelte-mode . ("svelteserver" "--stdio")))
  (add-to-list 'eglot-server-programs '(svelte-ts-mode . ("svelteserver" "--stdio"))))
