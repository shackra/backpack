(require 'backpack-pouch)

(leaf emacs-lisp
  :when (gearp! :programming emacs-lisp)
  :doc "Emacs lisp programming language"
  :config
  (leaf lispy
    :ensure (lispy :host github :repo "enzuru/lispy" :ref "c2acc4bfc1add60c8679c1191fa1b10144f748eb")
    :hook ((emacs-lisp-mode-hook lisp-mode-hook lisp-interaction-mode-hook common-lisp-mode-hook) . lispy-mode)
    :setq
    (lispy-key-theme . '(special paredit c-digits))
    (lispy-close-quotes-at-end-p . t)
    (lispy-safe-copy . t)
    (lispy-safe-delete . t)
    (lispy-safe-paste . t)
    (lispy-delete-sexp-from-within . nil)))
