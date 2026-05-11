;; Declare tree-sitter languages needed by this gear.
;; doxygen is an auxiliary grammar that provides Javadoc highlighting
;; inside java-ts-mode buffers.  It does not define its own major mode.
(when (backpack-treesit-was-asked 'java)
  (backpack-treesit-recipe! java
    :ts-mode 'java-ts-mode
    :remap 'java-mode
    :url "https://github.com/tree-sitter/tree-sitter-java"
    :ext "\\.java\\'"
    :versions ((:until-abi 14 :revision "e10607b45ff745f5f876bfa3e94fbcc6b44bdc11")))
  (backpack-treesit-recipe! doxygen
    :url "https://github.com/tree-sitter-grammars/tree-sitter-doxygen"
    :versions ((:until-abi 14 :revision "ccd998f378c3f9345ea4eeb223f56d7b84d16687"))
    :source-dir "src"))

(leaf cc-mode
  :doc "write once, debug anywhere"
  :when (gearp! :editing java)
  :mode ("\\.java\\'" . java-mode)
  :hook
  ((java-mode-hook java-ts-mode-hook) . electric-pair-local-mode)
  ((java-mode-hook java-ts-mode-hook) .
   (lambda ()
     (toggle-truncate-lines +1)
     (unless (gearp! :editing java -display-line-numbers)
       (display-line-numbers-mode +1)))))

(leaf java-ts-mode
  :doc "tree-sitter support for Java"
  :when (gearp! :editing java)
  :unless (gearp! :editing java -treesit)
  :after cc-mode)

(leaf eglot
  :doc "Language Server Protocol support for Java"
  :when (and (gearp! :editing java) (gearp! :editing java lsp))
  :doctor ("jdtls" . ("Eclipse JDT Language Server for Java development" required))
  :hook ((java-mode-hook java-ts-mode-hook) . eglot-ensure)
  :config
  ;; LSP protocol extensions note: Eclipse JDTLS supports non-standard
  ;; extensions for classpath updates, organize imports, override methods,
  ;; and code action proposals. Not yet implemented.
  (add-to-list 'eglot-server-programs '(java-mode . ("jdtls")))
  (add-to-list 'eglot-server-programs '(java-ts-mode . ("jdtls"))))
