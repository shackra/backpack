;;; run.el --- Entry point for `backpack bench' benchmark runs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jorge Javier Araya Navarro and Backpack contributors

;;; Commentary:

;; Loaded by `bench.el' (at repo root) under `emacs --batch'.  Responsible
;; for registering every benchmark, executing the suite, printing a
;; human-readable table to stdout, and appending a machine-readable
;; timestamped record under `.cache/etc/benchmarks/'.
;;
;; Optional environment variables:
;;   BACKPACK_BENCH_FILTER     -- substring; only run matching benches
;;   BACKPACK_BENCH_ITERATIONS -- override default per-variant iterations
;;   BACKPACK_BENCH_NO_SAVE    -- if set to a non-empty string, skip saving
;;                                the results file (useful in CI)

;;; Code:

(let ((bench-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path bench-dir))

(require 'backpack-bench-harness)
(require 'backpack-bench-defs)

;; `user-emacs-directory' has already been rebound by backpack-defaults.el to
;; point inside .cache/; `backpack-emacs-dir' is the real repo root.  We write
;; benchmark history next to the other Backpack data files.
(defvar backpack-bench-output-dir
  (expand-file-name "benchmarks" backpack-data-dir)
  "Directory under which timestamped benchmark result files are written.")

(defun backpack-bench--output-file ()
  "Return the path for the current run's result file."
  (expand-file-name
   (format "bench-%s.eld" (format-time-string "%Y%m%d-%H%M%S"))
   backpack-bench-output-dir))

(defun backpack-bench--maybe-override-iterations ()
  "Honor BACKPACK_BENCH_ITERATIONS, if set and numeric.

When set, the override forcibly replaces the iteration count of every
registered benchmark -- otherwise specs that declare their own
`:iterations' would silently ignore the flag, which is surprising
during development."
  (when-let* ((raw (getenv "BACKPACK_BENCH_ITERATIONS"))
              (n (and (string-match-p "\\`[0-9]+\\'" raw)
                      (string-to-number raw))))
    (setq backpack-bench-default-iterations n)
    (dolist (entry backpack-bench--spec-registry)
      (setcdr entry (plist-put (cdr entry) :iterations n)))
    (message "Overriding iterations to %d (every bench)" n)))

(defun backpack-bench--filter ()
  "Return the active filter string (from BACKPACK_BENCH_FILTER) or nil."
  (let ((raw (getenv "BACKPACK_BENCH_FILTER")))
    (and raw (not (string-empty-p raw)) raw)))

(defun backpack-bench--banner ()
  "Print a short environment banner to stdout."
  (message "")
  (message "========================================")
  (message "Backpack benchmark run")
  (message "========================================")
  (message "")
  (message "  emacs        : %s" emacs-version)
  (message "  system       : %s (%s)"
           system-configuration (symbol-name system-type))
  (message "  native-comp  : %s"
           (if (and (fboundp 'native-comp-available-p)
                    (native-comp-available-p))
               "yes" "no"))
  (message "  iterations   : %d (override via BACKPACK_BENCH_ITERATIONS)"
           backpack-bench-default-iterations)
  (when-let* ((f (backpack-bench--filter)))
    (message "  filter       : %s" f))
  (message ""))

(defun backpack-bench--execute ()
  (backpack-bench--maybe-override-iterations)
  (backpack-bench--banner)
  (message "Running benchmarks (A = stock defaults, B = :os windows gear):")
  (message "")
  (let* ((results (backpack-bench-run-all (backpack-bench--filter)))
         (table (backpack-bench-format-table results)))
    (message "")
    (message "Results:")
    (message "")
    (message "%s" table)
    (message "")
    (cond
     ((getenv "BACKPACK_BENCH_NO_SAVE")
      (message "(Skipping result file; BACKPACK_BENCH_NO_SAVE is set.)"))
     (t
      (let ((out (backpack-bench--output-file)))
        (backpack-bench-save-results results out)
        (message "History written to: %s" out))))
    (message "")
    results))

(backpack-bench--execute)

(provide 'run)

;;; run.el ends here
