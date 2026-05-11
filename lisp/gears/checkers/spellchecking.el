;; -*- lexical-binding: t; -*-

;;; Backend resolution
;;
;; The spellchecking gear supports two backends:
;;
;;   jinx     -- a fast just-in-time spell-checker backed by Enchant.
;;               Requires a C compiler and Enchant development headers
;;               on this machine the first time `jinx-mode' runs,
;;               because jinx ships its spellcheck primitives as a C
;;               dynamic module that it compiles lazily.
;;
;;   flyspell -- Emacs's built-in spell-checker driving an external
;;               `hunspell' process.  No dynamic module, no compiler,
;;               no Enchant -- just a binary on PATH.  The fallback
;;               used when jinx cannot possibly build (e.g. on
;;               locked-down Windows laptops where Enchant + a build
;;               toolchain are not installable).
;;
;; At gear load time we probe which backend is feasible and cache the
;; answer for the rest of the session.  Users can force the fallback
;; even on a machine where jinx would work by adding the `-jinx' flag
;; to the gear: `(gear! :checkers (spellchecking -jinx))'.

(defvar backpack--spellchecking-backend 'unresolved
  "Cached backend choice for the spellchecking gear.
One of the symbols `jinx', `flyspell', `none', or `unresolved'
\(the initial sentinel value before `backpack-spellchecking-backend'
has been called for the first time).")

(defun backpack--spellchecking-jinx-feasible-p ()
  "Return non-nil iff jinx's dynamic module can plausibly build and load.
Checks: dynamic modules supported by this Emacs, a C compiler on
`exec-path', and Enchant development headers discoverable either
via pkg-config or at the standard Unix include paths.  Never
invokes the compiler itself."
  (and module-file-suffix
       (seq-find #'executable-find '("gcc" "clang" "cc"))
       (or (and (executable-find "pkg-config")
                (zerop (call-process "pkg-config" nil nil nil
                                     "--exists" "enchant-2")))
           (file-readable-p "/usr/include/enchant-2/enchant.h")
           (file-readable-p "/usr/local/include/enchant-2/enchant.h"))))

(defun backpack--spellchecking-hunspell-feasible-p ()
  "Return non-nil iff `hunspell' is reachable and has at least one dictionary.
Runs `hunspell -D' once to enumerate available dictionaries; if
that section is empty we treat the backend as infeasible so we do
not activate `flyspell-mode' against a dictionary-less hunspell
\(which would error in every buffer on first word check)."
  (when-let* ((exe (executable-find "hunspell")))
    (with-temp-buffer
      (when (zerop (call-process exe nil t nil "-D"))
        (goto-char (point-min))
        ;; `hunspell -D' prints a SEARCH PATH block, then an
        ;; "AVAILABLE DICTIONARIES" header, then one dict per line,
        ;; then a "LOADED DICTIONARY" footer.  A non-empty section
        ;; between the header and the footer means at least one
        ;; dictionary is loadable.
        (and (re-search-forward "AVAILABLE DICTIONARIES" nil t)
             (progn (forward-line 1)
                    (not (or (eobp)
                             (looking-at-p "LOADED DICTIONARY:")))))))))

(defun backpack-spellchecking-backend ()
  "Resolve and cache the spell-check backend to use this session.
Return value is one of the symbols `jinx', `flyspell', or `none'.
Honours the `-jinx' opt-out flag on the spellchecking gear:
when that flag is set, jinx is skipped even if it would otherwise
be feasible, and the resolver falls through to flyspell."
  (when (eq backpack--spellchecking-backend 'unresolved)
    (setq backpack--spellchecking-backend
          (cond
           ((and (not (gearp! :checkers spellchecking -jinx))
                 (backpack--spellchecking-jinx-feasible-p))
            'jinx)
           ((backpack--spellchecking-hunspell-feasible-p)
            'flyspell)
           (t 'none)))
    (message "Backpack spellchecking: backend=%s"
             backpack--spellchecking-backend))
  backpack--spellchecking-backend)

;;; Backends

(leaf jinx
  :doc "a fast just-in-time spell-checker (Enchant-backed).

Activates automatically when a C compiler and Enchant development
headers are available.  On machines where that toolchain is not
installable (e.g. corporate Windows laptops), the spellchecking
gear falls back to `flyspell' + `hunspell' -- see the flyspell
leaf below.  Add the `-jinx' flag to force the fallback even
where jinx would work: `(gear! :checkers (spellchecking -jinx))'."
  :ensure (jinx :ref "55c844066601ec1a4741ff8f459ff6321f6935b7")
  :when (and (gearp! :checkers spellchecking)
             (eq (backpack-spellchecking-backend) 'jinx))
  :doctor (("cc"         . "C compiler for jinx's dynamic module (gcc/clang/cc)")
           ("pkg-config" . "locates Enchant development headers"))
  :hook ((text-mode-hook prog-mode-hook) . jinx-mode)
  :bind ("C-c ." . jinx-correct)
  :custom
  (jinx-camel-modes . '(prog-mode))
  (jinx-delay . 0.01))

(leaf flyspell
  :doc "built-in spell-checker fallback driving hunspell directly.

Activates automatically when jinx is not feasible on this machine
but `hunspell' is on PATH with at least one dictionary installed.
Chocolatey's `hunspell.portable' package ships the binary without
dictionaries -- drop `en_US.aff' and `en_US.dic' (from LibreOffice
or Firefox dictionary extensions) next to `hunspell.exe', or set
the `DICPATH' environment variable to a folder containing them,
before launching Emacs."
  :when (and (gearp! :checkers spellchecking)
             (eq (backpack-spellchecking-backend) 'flyspell))
  :doctor ("hunspell" . "spell-check backend (on Windows: `choco install hunspell.portable')")
  :hook
  (text-mode-hook . flyspell-mode)
  (prog-mode-hook . flyspell-prog-mode)
  ;; `flyspell-correct-word-before-point' is a safe no-op when
  ;; `flyspell-mode' is off, so a global binding is harmless and
  ;; keeps C-c . as "correct the nearest misspelling" regardless
  ;; of which backend resolved.  It mirrors the jinx leaf above.
  :bind ("C-c ." . flyspell-correct-word-before-point)
  :custom
  ;; We resolved feasibility with `executable-find' above, so leaving
  ;; this as the bare name lets ispell re-resolve against `exec-path'
  ;; at runtime -- which is also what ispell expects.
  (ispell-program-name . "hunspell")
  (ispell-really-hunspell . t)
  (ispell-dictionary . "en_US"))
