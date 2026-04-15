;;; objc.el --- Objective-C language support -*- lexical-binding: t; -*-

(leaf cc-mode
  :doc "[NSLanguage sendMessage:@\"from the 80s with love\"]"
  :when (gearp! :editing objc)
  :hook
  (objc-mode-hook . electric-pair-local-mode)
  (objc-mode-hook .
		  (lambda ()
		    (toggle-truncate-lines +1)
	    (unless (gearp! :editing objc -display-line-numbers)
	      (display-line-numbers-mode +1)))))

(leaf eglot
  :doc "Language Server Protocol support for Objective-C"
  :when (and (gearp! :editing objc) (gearp! :editing objc lsp))
  :hook (objc-mode-hook . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               `(objc-mode . ,(eglot-alternatives '(("clangd")
                                                    ("ccls")))))
  :doctor
  ("clangd" . ("a language server that provides IDE-like features to editors, part of the LLVM project" required))
  ("ccls" . ("a C/C++/Objective-C language server powered by clang" optional)))
