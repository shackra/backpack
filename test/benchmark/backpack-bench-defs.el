;;; backpack-bench-defs.el --- Windows-focused benchmarks  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jorge Javier Araya Navarro and Backpack contributors

;;; Commentary:

;; This file defines the A/B benchmark pairs that exercise the variables
;; flipped by the `:os windows' gear.  The A side represents the stock
;; Emacs defaults; the B side represents Backpack's Windows-optimized
;; values.  On non-Windows hosts most W32 variables do not exist or have
;; no effect, so those benches will report ~0 delta -- that is by design
;; and serves as a cheap smoke check that the gear is not regressing
;; other platforms.
;;
;; Startup is measured by spawning a fresh child Emacs process: the two
;; variants differ only in whether the Windows performance knobs are
;; pre-set via --eval before the Backpack early-init.el runs.

;;; Code:

(require 'cl-lib)
(require 'backpack-bench-harness)

;; Declare Windows-only dynamic variables as special so `let'-binding them
;; on non-Windows hosts does not create a lexical binding and does not
;; produce "unused lexical variable" byte-compilation warnings.
(defvar w32-get-true-file-attributes)
(defvar w32-get-true-file-link-count)
(defvar w32-pipe-read-delay)
(defvar w32-pipe-buffer-size)
(defvar w32-quote-process-args)

;; ---------------------------------------------------------------------------
;; Fixtures
;; ---------------------------------------------------------------------------

;; Note: `user-emacs-directory' has been rebound by backpack-defaults.el to
;; live inside .cache/; `backpack-emacs-dir' is the real repo root and the
;; correct anchor for locating repo-relative fixtures like base-packages/.

(defvar backpack-bench--sample-files nil
  "Cached list of .el files used by the file-attribute benchmarks.")

(defun backpack-bench--collect-sample-files ()
  "Return (and cache) up to 200 .el files under `base-packages/'."
  (or backpack-bench--sample-files
      (setq backpack-bench--sample-files
            (let* ((root (expand-file-name "base-packages"
                                           backpack-emacs-dir))
                   (all (and (file-directory-p root)
                             (directory-files-recursively root "\\.el\\'"))))
              (seq-take (sort (copy-sequence all) #'string<) 200)))))

(defvar backpack-bench--directory-scan-root nil
  "Directory scanned by the `directory-files-recursively' benchmark.")

(defun backpack-bench--directory-scan-root ()
  "Return (and cache) the directory used for recursive-scan benches."
  (or backpack-bench--directory-scan-root
      (setq backpack-bench--directory-scan-root
            (expand-file-name "base-packages" backpack-emacs-dir))))

(defvar backpack-bench--probe-libs
  '("cl-lib" "subr-x" "seq" "map" "pcase" "project" "xref"
    "bytecomp" "json" "rx" "url" "dired" "diff" "imenu"
    "thingatpt" "ert" "benchmark" "package" "eldoc" "flymake")
  "Libraries probed by `bench-load-path-probe'.")

(defvar backpack-bench--random-names
  (let ((exts '(".el" ".py" ".rs" ".go" ".c" ".cpp" ".js" ".ts" ".tsx"
                ".html" ".css" ".json" ".yaml" ".toml" ".md" ".txt"
                ".lua" ".hs" ".sh" ".nix")))
    (cl-loop for i from 0 below 500
             collect (format "/tmp/project/sub%d/file-%d%s"
                             (mod i 20) i
                             (nth (mod (* i 7) (length exts)) exts))))
  "Deterministic list of 500 synthetic filenames used for auto-mode matching.")

(defvar backpack-bench--large-output-file nil
  "Path to the file used for `bench-subprocess-large-output'.")

(defun backpack-bench--ensure-large-file ()
  "Create (if needed) and return the ~2.2 MB fixture file."
  (or (and backpack-bench--large-output-file
           (file-exists-p backpack-bench--large-output-file)
           backpack-bench--large-output-file)
      (let* ((dir (expand-file-name "bench" backpack-nonessential-dir))
             (file (expand-file-name "large-output.txt" dir)))
        (make-directory dir t)
        (unless (and (file-exists-p file)
                     (> (file-attribute-size (file-attributes file))
                        (* 2 1024 1024)))
          (with-temp-file file
            (dotimes (_ 50000)
              (insert "the quick brown fox jumps over the lazy dog\n"))))
        (setq backpack-bench--large-output-file file)
        file)))

;; ---------------------------------------------------------------------------
;; Helpers used inside benchmark thunks
;; ---------------------------------------------------------------------------

(defmacro backpack-bench--with-w32-file-attrs (attrs-val link-val &rest body)
  "Bind W32 file-attribute variables to ATTRS-VAL and LINK-VAL around BODY.

On non-Windows hosts, the let-bindings still take effect but have no
observable impact; the C-level code simply does not consult them."
  (declare (indent 2))
  `(let ((w32-get-true-file-attributes ,attrs-val)
         (w32-get-true-file-link-count ,link-val))
     ,@body))

(defmacro backpack-bench--with-process-io (pipe-delay pipe-buf conn-type
                                                     adaptive read-max
                                                     &rest body)
  "Bind Windows process IO knobs around BODY."
  (declare (indent 5))
  `(let ((w32-pipe-read-delay ,pipe-delay)
         (w32-pipe-buffer-size ,pipe-buf)
         (process-connection-type ,conn-type)
         (process-adaptive-read-buffering ,adaptive)
         (read-process-output-max ,read-max))
     ,@body))

;; ---------------------------------------------------------------------------
;; 1. file-attributes -- w32-get-true-file-attributes / -link-count
;; ---------------------------------------------------------------------------

(backpack-bench-defbench bench-file-attributes
  :doc "200 `file-attributes' calls over .el files.
A: stock (t / t)   B: Windows-optimized ('local / nil)"
  :iterations 15
  :setup (lambda () (backpack-bench--collect-sample-files))
  :run-a (lambda ()
           (backpack-bench--with-w32-file-attrs t t
             (dolist (f (backpack-bench--collect-sample-files))
               (file-attributes f))))
  :run-b (lambda ()
           (backpack-bench--with-w32-file-attrs 'local nil
             (dolist (f (backpack-bench--collect-sample-files))
               (file-attributes f)))))

;; ---------------------------------------------------------------------------
;; 2. directory-files-recursively -- full tree walk
;; ---------------------------------------------------------------------------

(backpack-bench-defbench bench-directory-scan
  :doc "`directory-files-recursively' over base-packages/.
A: stock     B: Windows-optimized w32 file-attribute vars"
  :iterations 10
  :setup (lambda () (backpack-bench--directory-scan-root))
  :run-a (lambda ()
           (backpack-bench--with-w32-file-attrs t t
             (directory-files-recursively
              (backpack-bench--directory-scan-root) "\\.el\\'")))
  :run-b (lambda ()
           (backpack-bench--with-w32-file-attrs 'local nil
             (directory-files-recursively
              (backpack-bench--directory-scan-root) "\\.el\\'"))))

;; ---------------------------------------------------------------------------
;; 3. load-path probing -- file-name-handler-alist / auto-mode-case-fold
;; ---------------------------------------------------------------------------

(defun backpack-bench--do-probe-libs (handler-alist case-fold)
  "Helper: probe every library in `backpack-bench--probe-libs'.

HANDLER-ALIST is bound as `file-name-handler-alist' and CASE-FOLD as
`auto-mode-case-fold' across the probes."
  (let ((file-name-handler-alist handler-alist)
        (auto-mode-case-fold case-fold))
    (dolist (lib backpack-bench--probe-libs)
      (locate-library lib))))

(defun backpack-bench--stock-handler-alist ()
  "Return the full default `file-name-handler-alist'.
Backpack strips this list at startup (see the Doom-style hack in
`lisp/backpack.el'); the original is preserved on the variable's
`initial-value' property.  Falls back to the current top-level value
if the property is missing (e.g. when running this file standalone)."
  (or (get 'file-name-handler-alist 'initial-value)
      (default-toplevel-value 'file-name-handler-alist)))

(backpack-bench-defbench bench-load-path-probe
  :doc "`locate-library' for 20 core libraries.
A: full file-name-handler-alist, case-fold t
B: stripped handler list, case-fold nil"
  :iterations 20
  :run-a (lambda ()
           (backpack-bench--do-probe-libs
            (backpack-bench--stock-handler-alist) t))
  :run-b (lambda ()
           (backpack-bench--do-probe-libs nil nil)))

;; ---------------------------------------------------------------------------
;; 4. auto-mode-alist matching -- auto-mode-case-fold
;; ---------------------------------------------------------------------------
;;
;; This benchmark simulates the inner loop of `set-auto-mode' by walking
;; `auto-mode-alist' for each synthetic filename.  Windows native Emacs
;; pays the case-folding cost for every buffer visit, so toggling
;; `auto-mode-case-fold' (and therefore `case-fold-search' inside the
;; matcher) is a realistic microbench.

(defun backpack-bench--match-names (case-fold)
  (let ((case-fold-search case-fold))
    (dolist (fn backpack-bench--random-names)
      (cl-block match
        (dolist (entry auto-mode-alist)
          (when (and (consp entry)
                     (stringp (car entry))
                     (string-match (car entry) fn))
            (cl-return-from match)))))))

(backpack-bench-defbench bench-auto-mode-match
  :doc "Match 500 filenames against `auto-mode-alist'.
A: case-fold t    B: case-fold nil"
  :iterations 20
  :run-a (lambda () (backpack-bench--match-names t))
  :run-b (lambda () (backpack-bench--match-names nil)))

;; ---------------------------------------------------------------------------
;; 5. Subprocess spawn (many short calls) -- process-connection-type etc.
;; ---------------------------------------------------------------------------

(defun backpack-bench--platform-echo-args ()
  "Return (PROGRAM ARGS...) that prints a short string and exits."
  (cond
   ((eq system-type 'windows-nt)
    (list (or (getenv "ComSpec") "cmd.exe") "/c" "echo" "hello"))
   (t
    (list "sh" "-c" "printf hello"))))

(defconst backpack-bench--echo-spawn-count 20
  "Number of `echo' spawns per iteration of `bench-subprocess-echo'.

Windows CreateProcess is expensive (>20 ms per call on healthy
hardware, >200 ms with an active Defender scan).  Keeping the per-
iteration spawn count modest means the full suite finishes in a few
minutes even on slower machines.")

(backpack-bench-defbench bench-subprocess-echo
  :doc "Spawn a trivial `echo' N times synchronously (N=20).
A: stock pipe vars (slow delay, small buffer, pty where possible)
B: Windows-optimized pipe vars (0 delay, 256 KiB buf, pipes)"
  :iterations 8
  :warmup 1
  :run-a (lambda ()
           (backpack-bench--with-process-io 50 4096 t t 4096
             (let ((argv (backpack-bench--platform-echo-args)))
               (dotimes (_ backpack-bench--echo-spawn-count)
                 (with-temp-buffer
                   (apply #'call-process (car argv) nil t nil (cdr argv)))))))
  :run-b (lambda ()
           (backpack-bench--with-process-io 0 (* 256 1024) nil nil (* 1024 1024)
             (let ((argv (backpack-bench--platform-echo-args)))
               (dotimes (_ backpack-bench--echo-spawn-count)
                 (with-temp-buffer
                   (apply #'call-process (car argv) nil t nil (cdr argv))))))))

;; ---------------------------------------------------------------------------
;; 6. Subprocess large output -- pipe buffer / read-process-output-max
;; ---------------------------------------------------------------------------

(defun backpack-bench--platform-cat-args (file)
  "Return argv for dumping FILE to stdout on the current platform."
  (cond
   ((eq system-type 'windows-nt)
    (list (or (getenv "ComSpec") "cmd.exe") "/c" "type"
          (subst-char-in-string ?/ ?\\ file)))
   (t
    (list "sh" "-c" (format "cat %s" (shell-quote-argument file))))))

(backpack-bench-defbench bench-subprocess-large-output
  :doc "Read ~2.2 MB from a subprocess into a buffer.
A: 4 KiB pipe buffer, 4 KiB read-max, adaptive buffering on
B: 256 KiB pipe buffer, 1 MiB read-max, adaptive buffering off"
  :iterations 8
  :warmup 1
  :setup (lambda () (backpack-bench--ensure-large-file))
  :run-a (lambda ()
           (backpack-bench--with-process-io 50 4096 t t 4096
             (let ((argv (backpack-bench--platform-cat-args
                          (backpack-bench--ensure-large-file))))
               (with-temp-buffer
                 (apply #'call-process (car argv) nil t nil (cdr argv))))))
  :run-b (lambda ()
           (backpack-bench--with-process-io 0 (* 256 1024) nil nil (* 1024 1024)
             (let ((argv (backpack-bench--platform-cat-args
                          (backpack-bench--ensure-large-file))))
               (with-temp-buffer
                 (apply #'call-process (car argv) nil t nil (cdr argv)))))))

;; ---------------------------------------------------------------------------
;; 7. Startup -- spawn a fresh child Emacs through early-init.el
;; ---------------------------------------------------------------------------

(defun backpack-bench--emacs-binary ()
  "Return the path to the Emacs binary that invoked us."
  (expand-file-name invocation-name invocation-directory))

(defvar backpack-bench--startup-stock-eval
  "(setq user-emacs-directory %S)"
  "--eval expression for the stock-variant child Emacs.")

(defvar backpack-bench--startup-windows-eval
  "(progn (setq user-emacs-directory %S)
          (when (boundp 'w32-pipe-read-delay) (setq w32-pipe-read-delay 0))
          (when (boundp 'w32-pipe-buffer-size) (setq w32-pipe-buffer-size (* 256 1024)))
          (when (boundp 'w32-get-true-file-attributes) (setq w32-get-true-file-attributes 'local))
          (when (boundp 'w32-get-true-file-link-count) (setq w32-get-true-file-link-count nil))
          (when (boundp 'w32-quote-process-args) (setq w32-quote-process-args t))
          (setq auto-mode-case-fold nil
                fast-but-imprecise-scrolling t
                inhibit-compacting-font-caches t
                process-adaptive-read-buffering nil
                process-connection-type nil
                read-process-output-max (* 1024 1024)))"
  "--eval expression that pre-installs the `:os windows' gear knobs.")

(defun backpack-bench--run-startup (eval-form)
  "Spawn a batch child Emacs through Backpack's early-init.el.

The outer `backpack-bench--time' already measures wall clock, so this
function just has to run the subprocess synchronously and ignore its
output.  The child is invoked with `-Q' so its cost is entirely
attributable to Backpack's own code."
  (let* ((emacs (backpack-bench--emacs-binary))
         (eid (expand-file-name "." backpack-emacs-dir))
         (early-init (expand-file-name "early-init.el" backpack-emacs-dir)))
    (with-temp-buffer
      (apply #'call-process
             emacs nil t nil
             (list "--batch" "-Q"
                   "--eval" (format eval-form eid)
                   "-l" early-init
                   "--eval" "(kill-emacs 0)")))))

(backpack-bench-defbench bench-startup
  :doc "Spawn a child `emacs --batch -Q' and run Backpack's early-init.el.
A: no Windows perf knobs set
B: Windows perf knobs pre-installed via --eval"
  :iterations 5
  :warmup 1
  :run-a (lambda ()
           (backpack-bench--run-startup
            backpack-bench--startup-stock-eval))
  :run-b (lambda ()
           (backpack-bench--run-startup
            backpack-bench--startup-windows-eval)))

(provide 'backpack-bench-defs)

;;; backpack-bench-defs.el ends here
