;;; cpp.el --- C++ language support -*- lexical-binding: t; -*-

;; Declare tree-sitter languages needed by this gear
(when (and (gearp! :editing cpp)
           (not (gearp! :editing cpp -treesit)))
  (backpack-treesit-langs! cpp)

  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode)))

(leaf cc-mode
  :doc "C but with templates that make the compiler question its existence"
  :when (gearp! :editing cpp)
  :hook
  ((c++-mode-hook c++-ts-mode-hook) . electric-pair-local-mode)
  ((c++-mode-hook c++-ts-mode-hook) .
   (lambda ()
     (toggle-truncate-lines +1)
     (unless (gearp! :editing cpp -display-line-numbers)
       (display-line-numbers-mode +1))))
  :config
  (leaf eglot
    :doc "Language Server Protocol support for C++"
    :when (gearp! :editing cpp lsp)
    :hook ((c++-mode-hook c++-ts-mode-hook) . eglot-ensure)
    :config
    (add-to-list 'eglot-server-programs
                 `(c++-mode . ,(eglot-alternatives '(("clangd")
                                                     ("ccls")))))
    (add-to-list 'eglot-server-programs
                 `(c++-ts-mode . ,(eglot-alternatives '(("clangd")
                                                        ("ccls")))))
    :doctor
    ("clangd" . ("a language server that provides IDE-like features to editors, part of the LLVM project" required))
    ("ccls" . ("a C/C++/Objective-C language server powered by clang" optional))))
