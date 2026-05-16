;; -*- no-byte-compile: t; -*-

(let (file-name-handler-alist)
  (if noninteractive
      (setq gc-cons-threshold (* 128 1024 1024)
	    gc-cons-percentage 1.0)
    (setq gc-cons-threshold most-positive-fixnum))

  (setq load-prefer-newer noninteractive)

  (let ((load-suffixes '(".elc" ".el"))
	(backpack (expand-file-name "lisp/backpack" user-emacs-directory)))
    (when (file-exists-p (concat backpack ".el"))
      ;; load backpack
      (load backpack nil nil nil t))
    (setq user-init-file (expand-file-name "early-init" user-emacs-directory))
    (setq load-prefer-newer t)
    (if backpack--system-windows-p
	;; ─── Garbage collection ───
	;; Windows pipe/IPC buffers are large (256 KB + 1 MiB read-max);
	;; the default 16 MiB post-init threshold triggers GC too often
	;; during LSP indexing bursts.  64 MiB is a good balance.
	;; See: https://emacs-lsp.github.io/lsp-mode/page/performance/
	;;      https://emacsredux.com/blog/2025/03/28/speed-up-emacs-startup-by-tweaking-the-gc-settings/
	(setq gc-cons-threshold (* 64 1024 1024))
      (setq gc-cons-threshold (* 16 1024 1024)))))

(backpack-start (not noninteractive))
