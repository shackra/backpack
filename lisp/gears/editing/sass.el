(leaf sass-mode
  :doc "Sass indented syntax support for Emacs"
  :when (gearp! :editing sass)
  :ensure (sass-mode :ref "247a0d4b509f10b28e4687cd8763492bca03599b")
  :mode "\\.sass\\'"
  :hook
  (sass-mode-hook . electric-pair-local-mode)
  (sass-mode-hook .
		  (lambda ()
		    (toggle-truncate-lines +1)
		    (unless (gearp! :editing sass -display-line-numbers)
		      (display-line-numbers-mode +1)))))
