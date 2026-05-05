;;; backpack-bench-harness.el --- Benchmark harness for Backpack  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jorge Javier Araya Navarro and Backpack contributors

;;; Commentary:

;; Small wrapper around `benchmark.el' that adds:
;;
;;   * warm-up iterations (discarded), so the JIT/native-comp path is hot
;;   * per-sample GC so one outlier cannot poison subsequent samples
;;   * min / median / mean / stddev reporting
;;   * an A/B runner that runs each benchmark twice -- once against the
;;     stock Emacs defaults, once against the Backpack Windows-optimized
;;     values -- and reports the delta, which is the interesting number.
;;
;; The harness is batch-safe (no hard dependency on a GUI frame) and
;; platform-agnostic.  Benchmarks that only make sense on Windows can
;; advertise an `:available-p' predicate; on other platforms they are
;; reported as skipped rather than producing meaningless numbers.

;;; Code:

(require 'cl-lib)

(defvar backpack-bench-default-iterations 20
  "Default number of timed samples per benchmark variant.")

(defvar backpack-bench-default-warmup 3
  "Default number of warm-up iterations (discarded) per variant.")

(defvar backpack-bench--spec-registry nil
  "Alist of (NAME . SPEC) for benchmarks registered via `backpack-bench-defbench'.")

(defmacro backpack-bench-defbench (name &rest spec)
  "Register a benchmark called NAME with SPEC plist.

Supported keys:
  :doc          documentation string
  :iterations   integer (overrides `backpack-bench-default-iterations')
  :warmup       integer (overrides `backpack-bench-default-warmup')
  :available-p  thunk -- return non-nil if the benchmark can run here
  :setup        thunk run once before each variant (may create fixtures)
  :run-a        thunk measuring the stock-defaults variant
  :run-b        thunk measuring the Windows-optimized variant

Both :run-a and :run-b take no arguments and return an arbitrary
value (ignored)."
  (declare (indent 1))
  `(progn
     (setf (alist-get ',name backpack-bench--spec-registry)
           (list :name ',name ,@spec))
     ',name))

(defun backpack-bench--median (samples)
  "Return the median of SAMPLES (a non-empty list of numbers)."
  (let* ((sorted (sort (copy-sequence samples) #'<))
         (n (length sorted)))
    (if (cl-oddp n)
        (float (nth (/ n 2) sorted))
      (/ (+ (nth (1- (/ n 2)) sorted)
            (nth (/ n 2) sorted))
         2.0))))

(defun backpack-bench--stats (samples)
  "Return a plist of :min :median :mean :stddev :n for SAMPLES."
  (let* ((n (length samples))
         (total (cl-reduce #'+ samples :initial-value 0.0))
         (mean (if (zerop n) 0.0 (/ total n)))
         (variance (if (< n 2)
                       0.0
                     (/ (cl-reduce
                         (lambda (acc x) (+ acc (expt (- x mean) 2)))
                         samples :initial-value 0.0)
                        (float (1- n))))))
    (list :min (if samples (apply #'min samples) 0.0)
          :max (if samples (apply #'max samples) 0.0)
          :median (if samples (backpack-bench--median samples) 0.0)
          :mean mean
          :stddev (sqrt variance)
          :n n)))

(defun backpack-bench--time (thunk iterations warmup)
  "Run THUNK ITERATIONS times after WARMUP discarded runs.
Return a list of wall-clock seconds per run."
  (dotimes (_ warmup)
    (funcall thunk))
  (let (samples)
    (dotimes (_ iterations)
      (garbage-collect)
      (let ((t0 (current-time)))
        (funcall thunk)
        (push (float-time (time-subtract (current-time) t0)) samples)))
    (nreverse samples)))

(defun backpack-bench--platform-skip-p (spec)
  "Return non-nil if SPEC declares it cannot run on the current platform."
  (let ((pred (plist-get spec :available-p)))
    (and pred (not (funcall pred)))))

(defun backpack-bench-run (spec)
  "Run benchmark SPEC (plist) and return a result plist.

Return value has the shape:

  (:name NAME :doc DOC :iterations N
   :a (:min .. :median .. :mean .. :stddev .. :n ..)
   :b (:min .. :median .. :mean .. :stddev .. :n ..))

or, if the benchmark is unavailable on the current platform,

  (:name NAME :skipped t :reason STRING)."
  (let* ((name (plist-get spec :name))
         (iter (or (plist-get spec :iterations)
                   backpack-bench-default-iterations))
         (warm (or (plist-get spec :warmup)
                   backpack-bench-default-warmup))
         (setup (plist-get spec :setup))
         (run-a (plist-get spec :run-a))
         (run-b (plist-get spec :run-b)))
    (message "  %-32s" (symbol-name name))
    (cond
     ((backpack-bench--platform-skip-p spec)
      (message "    skipped (not available on %s)" system-type)
      (list :name name :skipped t
            :reason (format "unavailable on %s" system-type)
            :doc (plist-get spec :doc)))
     ((not (and (functionp run-a) (functionp run-b)))
      (error "Benchmark %s is missing :run-a or :run-b" name))
     (t
      (when setup (funcall setup))
      (let* ((samples-a (backpack-bench--time run-a iter warm))
             (samples-b (backpack-bench--time run-b iter warm))
             (stats-a (backpack-bench--stats samples-a))
             (stats-b (backpack-bench--stats samples-b))
             (med-a (plist-get stats-a :median))
             (med-b (plist-get stats-b :median))
             (pct (if (> med-a 0)
                      (* 100.0 (/ (- med-b med-a) med-a))
                    0.0)))
        (message "    A median %8.3f ms   B median %8.3f ms   delta %+6.1f%%"
                 (* 1000 med-a) (* 1000 med-b) pct)
        (list :name name
              :doc (plist-get spec :doc)
              :iterations iter
              :warmup warm
              :a stats-a
              :b stats-b))))))

(defun backpack-bench--format-cell (value)
  "Format VALUE (seconds) for table output, as milliseconds."
  (format "%10.3f" (* 1000 value)))

(defun backpack-bench-format-table (results)
  "Return a pretty ASCII table string for RESULTS.

RESULTS is a list of plists as returned by `backpack-bench-run'."
  (let* ((header (format "%-32s %10s %10s %10s %10s %10s %8s"
                         "benchmark"
                         "A med ms" "A sd ms"
                         "B med ms" "B sd ms"
                         "delta ms" "delta %"))
         (divider (make-string (length header) ?-))
         (rows
          (mapcar
           (lambda (r)
             (cond
              ((plist-get r :skipped)
               (format "%-32s %s"
                       (symbol-name (plist-get r :name))
                       (or (plist-get r :reason) "(skipped)")))
              (t
               (let* ((a (plist-get r :a))
                      (b (plist-get r :b))
                      (med-a (plist-get a :median))
                      (med-b (plist-get b :median))
                      (delta (- med-b med-a))
                      (pct (if (> med-a 0)
                               (* 100.0 (/ delta med-a))
                             0.0)))
                 (format "%-32s %s %s %s %s %s %+7.1f%%"
                         (symbol-name (plist-get r :name))
                         (backpack-bench--format-cell med-a)
                         (backpack-bench--format-cell (plist-get a :stddev))
                         (backpack-bench--format-cell med-b)
                         (backpack-bench--format-cell (plist-get b :stddev))
                         (format "%+10.3f" (* 1000 delta))
                         pct)))))
           results)))
    (mapconcat #'identity
               (append (list header divider) rows)
               "\n")))

(defun backpack-bench-save-results (results output-file)
  "Save RESULTS to OUTPUT-FILE as a single readable plist.

The file also embeds environment metadata (timestamp, system type,
Emacs version, native-comp availability) so historical runs can be
diffed meaningfully."
  (make-directory (file-name-directory output-file) t)
  (let ((record
         (list :timestamp (format-time-string "%Y-%m-%dT%H:%M:%S%z")
               :system-type (symbol-name system-type)
               :system-configuration system-configuration
               :emacs-version emacs-version
               :native-compile (and (fboundp 'native-comp-available-p)
                                    (native-comp-available-p))
               :host (system-name)
               :results results)))
    (with-temp-file output-file
      (let ((print-level nil)
            (print-length nil)
            (print-circle t))
        (insert ";; -*- mode: emacs-lisp; -*-\n")
        (insert ";; Backpack benchmark run\n")
        (insert (format ";; %s -- %s / Emacs %s\n\n"
                        (plist-get record :timestamp)
                        (plist-get record :system-type)
                        (plist-get record :emacs-version)))
        (pp record (current-buffer))))
    output-file))

(defun backpack-bench-run-all (&optional filter)
  "Run every registered benchmark and return the list of result plists.

If FILTER is a non-nil symbol or string, only benchmarks whose name
contains FILTER (as a substring) are executed.  Useful for iterating
on one bench without paying for the whole suite."
  (let ((results '()))
    (dolist (entry backpack-bench--spec-registry)
      (let* ((spec (cdr entry))
             (name (symbol-name (plist-get spec :name))))
        (when (or (null filter)
                  (string-match-p (if (symbolp filter)
                                      (symbol-name filter)
                                    filter)
                                  name))
          (push (backpack-bench-run spec) results))))
    (nreverse results)))

(provide 'backpack-bench-harness)

;;; backpack-bench-harness.el ends here
