;; Declare tree-sitter languages needed by this gear
(when (and (gearp! :editing terraform)
	   (not (gearp! :editing terraform -treesit))
	   ;; NOTE(shackra): disabled until I can figure out the reason for this error: Ignoring unknown mode ‘terraform-mode’ (remapped to `terraform-ts-mode')
	   nil)
  (backpack-treesit-langs! terraform)

  (with-eval-after-load 'treesit-auto
    (add-to-list 'treesit-auto-recipe-list
		 (make-treesit-auto-recipe
		  :lang 'terraform
		  :ts-mode 'terraform-ts-mode
		  :url "https://github.com/tree-sitter-grammars/tree-sitter-hcl"
		  :source-dir "dialects/terraform/src")))

  (add-to-list 'major-mode-remap-alist '(terraform-mode . terraform-ts-mode)))

(leaf terraform-mode
  :doc "A major-mode for editing Terraform configuration files"
  :ensure (terraform-mode :ref "01635df3625c0cec2bb4613a6f920b8569d41009")
  :when (gearp! :editing terraform)
  :hook
  ((terraform-mode-hook terraform-ts-mode-hook) . electric-pair-local-mode)
  ((terraform-mode-hook terraform-ts-mode-hook) .
   (lambda ()
     (toggle-truncate-lines +1)
     (unless (gearp! :editing terraform -display-line-numbers)
       (display-line-numbers-mode +1))))
  :config
  (leaf eglot
    :doc "Language Server Protocol support for terraform-mode"
    :when (gearp! :editing terraform lsp)
    :doctor
    ("terraform-ls" . "the official Terraform Language Server by HashiCorp")
    :hook ((terraform-mode-hook terraform-ts-mode-hook) . eglot-ensure)
    :config
    (add-to-list 'eglot-server-programs '(terraform-mode . ("terraform-ls" "serve")))
    (add-to-list 'eglot-server-programs '(terraform-ts-mode . ("terraform-ls" "serve")))))

(leaf terraform-doc
  :doc "Look up Terraform documentation from Emacs"
  :when (gearp! :editing terraform doc)
  :ensure (terraform-doc :ref "31f1c47453ad14181883f78258a72c02b95d9783"))
