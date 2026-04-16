;; Declare tree-sitter languages needed by this gear.
;; The HCL grammar hosts Terraform as a dialect under dialects/terraform/src.
(when (and (gearp! :editing terraform)
	   (not (gearp! :editing terraform -treesit)))
  (backpack-treesit-recipe! terraform
    :ts-mode 'terraform-ts-mode
    :remap 'terraform-mode
    :url "https://github.com/tree-sitter-grammars/tree-sitter-hcl"
    :source-dir "dialects/terraform/src"
    :versions ((:until-emacs "29.4" :revision "422cbe1d93a8ae3847744b664041fc76876babcd"))))

(leaf terraform-mode
  :doc "plan, apply, pray -- infrastructure as code"
  :ensure (terraform-mode :ref "01635df3625c0cec2bb4613a6f920b8569d41009")
  :when (gearp! :editing terraform)
  :hook
  ((terraform-mode-hook terraform-ts-mode-hook) . electric-pair-local-mode)
  ((terraform-mode-hook terraform-ts-mode-hook) .
   (lambda ()
     (toggle-truncate-lines +1)
     (unless (gearp! :editing terraform -display-line-numbers)
       (display-line-numbers-mode +1)))))

(leaf eglot
  :doc "Language Server Protocol support for terraform-mode"
  :when (and (gearp! :editing terraform) (gearp! :editing terraform lsp))
  :doctor
  ("terraform-ls" . "the official Terraform Language Server by HashiCorp")
  :hook ((terraform-mode-hook terraform-ts-mode-hook) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(terraform-mode . ("terraform-ls" "serve")))
  (add-to-list 'eglot-server-programs '(terraform-ts-mode . ("terraform-ls" "serve"))))

(leaf terraform-ts-mode
  :doc "tree-sitter support for Terraform"
  :ensure (terraform-ts-mode :host github :repo "kgrotel/terraform-ts-mode" :ref "985ed2a65dfdddcd50c5efd52975caff10ffb9d2")
  :when (and (gearp! :editing terraform)
	     (not (gearp! :editing terraform -treesit)))
  :require t)

(leaf terraform-doc
  :doc "Look up Terraform documentation from Emacs"
  :when (gearp! :editing terraform doc)
  :ensure (terraform-doc :ref "31f1c47453ad14181883f78258a72c02b95d9783"))
