;; -*- no-byte-compile: t; -*-

(let (file-name-handler-alist)
  (if noninteractive
      (setq gc-cons-threshold (* 128 1024 1024)
	    gc-cons-percentage 1.0)
    (setq gc-cons-threshold most-positive-fixnum))

  (setq load-prefer-newer noninteractive)

  (if (let ((load-suffixes '(".elc" ".el"))
	    (backpack (expand-file-name "lisp/backpack" user-emacs-directory)))
	(if (file-exists-p (concat backpack ".el"))
	    ;; load backpack
	    (load backpack nil nil nil t)
	  (warn "no file lisp/backpack.el found"))
	(setq user-init-file (expand-file-name "early-init" user-emacs-directory))
	(setq load-prefer-newer t)
	(setq gc-cons-threshold (* 16 1024 1024))
	nil)
      ;; (doom-initialize (not noninteractive))
      (load user-init-file 'noerror nil nil 'must-suffix)))
