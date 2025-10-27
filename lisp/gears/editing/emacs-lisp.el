(leaf emacs-lisp
  :when (gearp! :editing emacs-lisp)
  :doc "Emacs lisp programming language"
  :hook
  ((emacs-lisp-mode-hook lisp-mode-hook) .
   (lambda ()
     (toggle-truncate-lines +1)

     (unless (gearp! :editing emacs-lisp -display-line-numbers)
       (display-line-numbers-mode +1)))))

(leaf lispy
  :when (gearp! :editing emacs-lisp)
  :ensure (lispy :host github :repo "enzuru/lispy" :ref "c2acc4bfc1add60c8679c1191fa1b10144f748eb")
  :hook ((emacs-lisp-mode-hook lisp-mode-hook lisp-interaction-mode-hook) . lispy-mode)
  :setq
  (lispy-key-theme . '(special paredit c-digits))
  (lispy-close-quotes-at-end-p . t)
  (lispy-safe-copy . t)
  (lispy-safe-delete . t)
  (lispy-safe-paste . t)
  (lispy-delete-sexp-from-within . nil))
