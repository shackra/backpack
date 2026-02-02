;; Declare tree-sitter languages needed by this gear
(when (and (gearp! :editing json)
           (not (gearp! :editing json -treesit)))
  (backpack-treesit-langs! json))

(leaf json
  :doc "major mode for editing JSON files"
  :when (gearp! :editing json)
  :hook
  (js-json-mode-hook . electric-pair-local-mode)
  (js-json-mode-hook .
		  (lambda ()
		    (toggle-truncate-lines 1)
		    (unless (gearp! :editing json -display-line-numbers)
		      (display-line-numbers-mode +1))))
  :config
  (leaf eglot
    :when (gearp! :editing json lsp)
    :doc "Language Server Protocol support for JSON"
    :hook
    (js-json-mode-hook . eglot-ensure)))

(leaf json-ts-mode
  :doc "treesit support for editing JSON files"
  :when (and (gearp! :editing json) (not (gearp! :editing json -treesit)))
  :config
  (add-to-list 'major-mode-remap-alist '(js-json-mode . json-ts-mode))
  (setq json-ts-mode-hook js-json-mode-hook))
