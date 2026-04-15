;; Declare tree-sitter languages needed by this gear
(when (and (gearp! :editing json)
           (not (gearp! :editing json -treesit)))
  (backpack-treesit-langs! json))

(leaf json
  :doc "the format that mass-produces missing commas and trailing comma debates"
  :when (gearp! :editing json)
  :hook
  ((js-json-mode-hook json-ts-mode-hook) . electric-pair-local-mode)
  ((js-json-mode-hook json-ts-mode-hook) .
   (lambda ()
     (toggle-truncate-lines 1)
     (unless (gearp! :editing json -display-line-numbers)
       (display-line-numbers-mode +1)))))

(leaf eglot
  :doc "Language Server Protocol support for JSON"
  :when (and (gearp! :editing json) (gearp! :editing json lsp))
  :hook
  ((js-json-mode-hook json-ts-mode-hook) . eglot-ensure))

(leaf json-ts-mode
  :doc "treesit support for editing JSON files"
  :unless (gearp! :editing json -treesit)
  :after json)
