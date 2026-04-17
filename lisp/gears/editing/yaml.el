;; Declare tree-sitter languages needed by this gear
(when (and (gearp! :editing yaml)
           (not (gearp! :editing yaml -treesit)))
  (backpack-treesit-recipe! yaml
    :ts-mode 'yaml-ts-mode
    :remap 'yaml-mode
    :url "https://github.com/tree-sitter-grammars/tree-sitter-yaml"
    :ext "\\.ya?ml\\'"
    :versions ((:until-emacs "29.4" :revision "7b03feefd36b5f155465ca736c6304aca983b267"))))

(leaf yaml-mode
  :doc "indentation-sensitive and ready to ruin your CI pipeline"
  :ensure (yaml-mode :ref "d91f878729312a6beed77e6637c60497c5786efa")
  :when (gearp! :editing yaml)
  :mode "\\.ya?ml\\'"
  :hook
  ((yaml-mode-hook yaml-ts-mode-hook) . electric-pair-local-mode)
  ((yaml-mode-hook yaml-ts-mode-hook) .
   (lambda ()
     (toggle-truncate-lines +1)
     (setq-local indent-tabs-mode nil)
     (unless (gearp! :editing yaml -display-line-numbers)
       (display-line-numbers-mode +1))))
  (yaml-ts-mode-hook .
   (lambda ()
     (setq-local indent-line-function 'yaml-indent-line))))

(leaf eglot
  :doc "Language Server Protocol support for yaml-mode.
Flags: lsp (enable yaml-language-server), -yaml-ls-ext (disable
yaml-language-server protocol extensions for schema selection)"
  :when (and (gearp! :editing yaml) (gearp! :editing yaml lsp))
  :doctor
  ("yaml-language-server" . "a Language Server for YAML files")
  :hook ((yaml-mode-hook yaml-ts-mode-hook) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(yaml-mode . ("yaml-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(yaml-ts-mode . ("yaml-language-server" "--stdio")))
  ;; Load yaml-language-server protocol extensions (schema selection support)
  ;; unless the user opts out with the -yaml-ls-ext flag.
  (unless (gearp! :editing yaml -yaml-ls-ext)
    (require 'backpack-yaml-ls)))
