;;; c.el --- C language support -*- lexical-binding: t; -*-

;; Declare tree-sitter languages needed by this gear
(when (and (gearp! :editing c)
           (not (gearp! :editing c -treesit)))
  (backpack-treesit-recipe! c
    :ts-mode 'c-ts-mode
    :remap 'c-mode
    :url "https://github.com/tree-sitter/tree-sitter-c"
    :ext "\\.c\\'"
    :versions ((:until-abi 14 :revision "deca017a554045b4c203e7ddff39ae64ff05e071"))))

(leaf cc-mode
  :doc "the language that mass-produces segfaults since 1972"
  :when (gearp! :editing c)
  :hook
  ((c-mode-hook c-ts-mode-hook) . electric-pair-local-mode)
  ((c-mode-hook c-ts-mode-hook) .
   (lambda ()
     (toggle-truncate-lines +1)
     (unless (gearp! :editing c -display-line-numbers)
       (display-line-numbers-mode +1)))))

(leaf eglot
  :doc "Language Server Protocol support for C"
  :when (and (gearp! :editing c) (gearp! :editing c lsp))
  :hook ((c-mode-hook c-ts-mode-hook) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               `(c-mode . ,(eglot-alternatives '(("clangd")
                                                 ("ccls")))))
  (add-to-list 'eglot-server-programs
               `(c-ts-mode . ,(eglot-alternatives '(("clangd")
                                                    ("ccls")))))
  :doctor
  ("clangd" . ("a language server that provides IDE-like features to editors, part of the LLVM project" required))
  ("ccls" . ("a C/C++/Objective-C language server powered by clang" optional)))
