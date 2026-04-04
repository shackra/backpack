;;; cmake.el --- CMake support -*- lexical-binding: t; -*-

;; Declare tree-sitter languages needed by this gear
(when (and (gearp! :editing cmake)
           (not (gearp! :editing cmake -treesit)))
  (backpack-treesit-langs! cmake)

  (add-to-list 'major-mode-remap-alist '(cmake-mode . cmake-ts-mode)))

(leaf cmake-mode
  :doc "Support for editing CMake source files in Emacs"
  :when (gearp! :editing cmake)
  :ensure (cmake-mode :ref "1772622772133fad3b348ca4a5b4df3bbd69da75")
  :hook
  ((cmake-mode-hook cmake-ts-mode-hook) . electric-pair-local-mode)
  ((cmake-mode-hook cmake-ts-mode-hook) .
   (lambda ()
     (toggle-truncate-lines +1)
     (unless (gearp! :editing cmake -display-line-numbers)
       (display-line-numbers-mode +1))))
  :config
  (leaf eglot
    :doc "Language Server Protocol support for CMake"
    :when (gearp! :editing cmake lsp)
    :doctor ("cmake-language-server" . "a language server for CMake that provides completion, diagnostics and documentation")
    :hook ((cmake-mode-hook cmake-ts-mode-hook) . eglot-ensure)
    :config
    (add-to-list 'eglot-server-programs '(cmake-mode . ("cmake-language-server")))
    (add-to-list 'eglot-server-programs '(cmake-ts-mode . ("cmake-language-server")))))
