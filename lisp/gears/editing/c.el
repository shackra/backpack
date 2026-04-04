;;; c.el --- C language support -*- lexical-binding: t; -*-

;; Declare tree-sitter languages needed by this gear
(when (and (gearp! :editing c)
           (not (gearp! :editing c -treesit)))
  (backpack-treesit-langs! c)

  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode)))

(leaf cc-mode
  :doc "Support for C"
  :when (gearp! :editing c)
  :hook
  ((c-mode-hook c-ts-mode-hook) . electric-pair-local-mode)
  ((c-mode-hook c-ts-mode-hook) .
   (lambda ()
     (toggle-truncate-lines +1)
     (unless (gearp! :editing c -display-line-numbers)
       (display-line-numbers-mode +1))))
  :config
  (leaf eglot
    :doc "Language Server Protocol support for C"
    :when (gearp! :editing c lsp)
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
    ("ccls" . ("a C/C++/Objective-C language server powered by clang" optional))))
