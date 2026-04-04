(leaf emacs-lisp
  :when (gearp! :editing emacs-lisp)
  :doc "the language you swore you'd never learn, and now you can't stop"
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

(leaf let-completion
  :doc "Let-binding values in Elisp completion"
  :when (gearp! :editing emacs-lisp)
  :ensure (let-completion :host github :repo "gggion/let-completion.el" :ref "ee8822e32ba9c0a81152a5dd9bf4e272655d7319")
  :hook ((emacs-lisp-mode-hook lisp-mode-hook lisp-interaction-mode-hook) . let-completion-mode)
  :setq let-completion-tag-kind-alist
  '((lambda              . "λ")
    (cl-function         . "𝘧")
    (function            . "𝘧")
    (make-hash-table     . "#s")
    (quote               . "'()")
    (cons                . "cons")
    (list                . "list")
    (make-vector         . "vec")
    (vector              . "vec")
    (rx                  . "rx")
    (rx-to-string        . "rx")
    (regexp-opt          . "rx")
    (format              . "str")
    (get-buffer-create   . "buffer")
    (make-process        . "proc")
    (start-process       . "proc")
    (make-pipe-process   . "proc")))
