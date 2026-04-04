;;; make.el --- GNU Make support -*- lexical-binding: t; -*-

(leaf make-mode
  :doc "tabs vs spaces? here there's only tabs. always tabs."
  :when (gearp! :editing make)
  :hook
  ((makefile-mode-hook makefile-gmake-mode-hook makefile-bsdmake-mode-hook) . electric-pair-local-mode)
  ((makefile-mode-hook makefile-gmake-mode-hook makefile-bsdmake-mode-hook) .
   (lambda ()
     (toggle-truncate-lines +1)
     (unless (gearp! :editing make -display-line-numbers)
       (display-line-numbers-mode +1)))))
