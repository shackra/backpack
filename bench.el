;;; bench.el --- Backpack benchmarking entry point  -*- lexical-binding: t; -*-
;;
;; Usage:  emacs --batch --eval "(setq user-emacs-directory \"/path/to/backpack/\")" -l bench.el
;;
;; Invoked by `bin/backpack bench' (POSIX) and `bin\backpack.cmd bench' (Windows).
;; The benchmark suite does *not* need elpaca or any user gears to be loaded:
;; it performs its own minimal setup so results are reproducible regardless of
;; what is pinned in the user's init.el.
;;
;; The actual benchmarks live under `test/benchmark/' so they participate in the
;; same code-review and history as the unit tests.

;;; Code:

;; Capture the original user-emacs-directory *before* loading backpack.el.
;; backpack-defaults.el deliberately rebinds `user-emacs-directory' to the
;; .cache/ subdir to keep $HOME tidy, which would break path resolution here
;; if we relied on it after the load.
(defvar backpack-bench--repo-root
  (file-name-as-directory (expand-file-name user-emacs-directory))
  "Absolute path to the Backpack source tree, captured before backpack.el loads.")

;; Mirror the path dance ensure.el / gc.el do, so base-packages libraries are
;; on the load-path for anything the benchmark harness might pull in.
(add-to-list 'load-path (expand-file-name "base-packages/leaf.el" backpack-bench--repo-root))
(add-to-list 'load-path (expand-file-name "base-packages/leaf-keywords.el" backpack-bench--repo-root))
(add-to-list 'load-path (expand-file-name "lisp" backpack-bench--repo-root))
(add-to-list 'load-path (expand-file-name "test/benchmark" backpack-bench--repo-root))

;; Suppress the "Elpaca is not installed" display-warning that backpack.el
;; emits in normal mode when the user hasn't run `backpack ensure' yet.
;; The benchmark suite doesn't need elpaca: its microbenches exercise
;; variables and Lisp primitives, not packages.  Requiring `warnings'
;; before mutating `warning-suppress-log-types' avoids the "defining as
;; dynamic an already lexical var" complaint under lexical-binding.
(require 'warnings)
(add-to-list 'warning-suppress-log-types '(backpack))
(let ((backpack-file (expand-file-name "lisp/backpack.el"
                                       backpack-bench--repo-root)))
  (load backpack-file nil nil nil t))

;; Kick off the suite.  run.el executes at load time and writes its own
;; output; we just forward its exit status.
(load (expand-file-name "test/benchmark/run.el" backpack-bench--repo-root)
      nil nil nil t)

(kill-emacs 0)

;;; bench.el ends here
