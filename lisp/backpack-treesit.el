;; -*- lexical-binding: t; -*-
;;; backpack-treesit.el --- Tree-sitter grammar management and introspection  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Jorge Javier Araya Navarro and Backpack contributors

;; Author: Jorge Javier Araya Navarro <jorge@esavara.cr>
;; URL: https://github.com/shackra/backpack

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tree-sitter grammar declaration, installation, state tracking, and
;; introspection.  Gear files use `backpack-treesit-langs!' and
;; `backpack-treesit-recipe!' to declare grammars; `backpack ensure'
;; calls `backpack--install-treesit-grammars' to build them.

;;; Code:

;; `treesit.el' ships with Emacs 29+ regardless of compile flags, but every
;; useful function it documents is implemented in C (treesit.c).  When Emacs
;; is built without `--with-tree-sitter', loading the elisp library succeeds
;; but the underlying C symbols are unbound.  Use `noerror' so we never block
;; startup just because the user happens to be on a non-treesit build.
(require 'treesit nil 'noerror)
(require 'backpack-pouch)

(defun backpack-treesit-available-p ()
  "Return non-nil when this Emacs build has tree-sitter support compiled in.
This checks both that the elisp library is present and that the
underlying C runtime (`treesit-available-p') is bound and reports
itself as available.  When this returns nil, every Backpack
treesit facility (recipe registration, grammar installation,
treesit-auto activation) becomes a no-op."
  (and (featurep 'treesit)
       (fboundp 'treesit-available-p)
       (treesit-available-p)))

;;; Tree-sitter language management
(defvar backpack--treesit-langs nil
  "List of tree-sitter language symbols that are needed by enabled gears.
This is populated by `backpack-treesit-langs!' calls in gear files.")

(defmacro backpack-treesit-langs! (&rest langs)
  "Declare that the current gear needs tree-sitter support for LANGS.
LANGS should be symbols like `go', `python', `json', etc.  These
will be added to `treesit-auto-langs' and installed during sync.

When the running Emacs has no tree-sitter support compiled in
\(see `backpack-treesit-available-p'), the macro expands to a
no-op so gear files do not pollute Backpack's bookkeeping with
languages we cannot use anyway."
  `(when (backpack-treesit-available-p)
     (dolist (lang ',langs)
       (cl-pushnew lang backpack--treesit-langs))))

(defun backpack--plist-remove (plist key)
  "Return a copy of PLIST with KEY (and its value) removed.
If KEY appears more than once only the first occurrence is removed."
  (let (result found)
    (while plist
      (let ((k (pop plist))
            (v (pop plist)))
        (if (and (not found) (eq k key))
            (setq found t)
          (setq result (nconc result (list k v))))))
    result))

(defun backpack--plist-merge (base override)
  "Return a new plist with BASE keys updated by OVERRIDE keys.
Keys present in OVERRIDE replace the same keys in BASE; keys present
only in BASE or only in OVERRIDE are preserved."
  (let ((result (copy-sequence base)))
    (while override
      (let ((k (pop override))
            (v (pop override)))
        (setq result (plist-put result k v))))
    result))

(defun backpack--treesit-clause-matches-p (clause)
  "Return non-nil if CLAUSE applies to the current treesit environment.

A clause is a plist gated by `:until-abi N': it matches when the
running Emacs's maximum supported tree-sitter ABI
\(`treesit-library-abi-version') is less than or equal to N.  In
other words, the clause activates on installations whose local
libtree-sitter cannot load grammars compiled to ABIs newer than N.

A clause without `:until-abi' never matches."
  (let ((until-abi (plist-get clause :until-abi)))
    (and until-abi
         (fboundp 'treesit-library-abi-version)
         (let ((max-abi (treesit-library-abi-version)))
           (and (numberp max-abi) (<= max-abi until-abi))))))

(defun backpack--treesit-resolve-recipe (base versions)
  "Return the effective recipe plist for BASE merged with the first matching clause.
BASE is a plist of recipe fields.  VERSIONS is a list of override
plists, each gated by `:until-abi N'.  The first clause for which
`backpack--treesit-clause-matches-p' returns non-nil wins; its
remaining keys (with `:until-abi' stripped) are merged over BASE,
replacing existing values.  Returns BASE unchanged when no clause
matches or VERSIONS is nil."
  (let* ((override (cl-find-if #'backpack--treesit-clause-matches-p versions)))
    (if override
        (backpack--plist-merge
         base
         (backpack--plist-remove override :until-abi))
      base)))

(defun backpack--treesit-recipe-to-plist (recipe)
  "Convert a `treesit-auto-recipe' struct to a keyword plist.
Only non-nil slots are included.  The `:lang' slot is omitted because
it is always set explicitly by `backpack-treesit-recipe!'."
  (let (result)
    (dolist (slot '((:ts-mode    . treesit-auto-recipe-ts-mode)
                    (:remap      . treesit-auto-recipe-remap)
                    (:requires   . treesit-auto-recipe-requires)
                    (:url        . treesit-auto-recipe-url)
                    (:revision   . treesit-auto-recipe-revision)
                    (:source-dir . treesit-auto-recipe-source-dir)
                    (:cc         . treesit-auto-recipe-cc)
                    (:c++        . treesit-auto-recipe-c++)
                    (:ext        . treesit-auto-recipe-ext)))
      (let ((val (funcall (cdr slot) recipe)))
        (when val
          (setq result (nconc result (list (car slot) val))))))
    result))

(defmacro backpack-treesit-recipe! (lang &rest args)
  "Register a treesit-auto recipe for LANG with optional version overrides.

ARGS is a plist of `make-treesit-auto-recipe' fields:
  :ts-mode    -- the tree-sitter major mode symbol
  :remap      -- mode(s) to remap to the ts-mode
  :url        -- git URL of the grammar repository
  :revision   -- branch or tag to check out (default: repository HEAD)
  :source-dir -- subdirectory inside the repo containing the grammar src/
  :ext        -- file extension regexp for auto-mode association

In addition ARGS may contain:

  :versions CLAUSES

where CLAUSES is a list of override plists.  Each override plist
must contain a `:until-abi N' gating key: the clause matches when
the running Emacs's maximum supported tree-sitter ABI
\(`treesit-library-abi-version') is less than or equal to N.  Use
this to pin grammars whose upstream regenerated `parser.c' to an
ABI newer than the local libtree-sitter supports (which causes a
`version-mismatch' load failure).

The first matching clause wins.  When no clause matches the base
fields are used unchanged.

Inside a clause, use `:revision' to pin to a specific commit hash
\(this replaces the base `:revision' branch/tag with the pinned ref).

Clause selection runs at the time treesit-auto loads (inside
`with-eval-after-load'), so the running Emacs's libtree-sitter
ABI range is used.

This macro also implicitly calls `backpack-treesit-langs!' for
LANG, so there is no need to call it separately.

Fields not supplied in ARGS are inherited from `treesit-auto's
built-in recipe for LANG (if one exists), so only overrides need
to be specified.

Example -- pin by ABI (any Emacs whose libtree-sitter caps at ABI 14):

  (backpack-treesit-recipe! yaml
    :url \"https://github.com/tree-sitter-grammars/tree-sitter-yaml\"
    :versions ((:until-abi 14 :revision \"abc123lastABI14commit\")))"
  (declare (indent 1))
  `(when (backpack-treesit-available-p)
     (backpack-treesit-langs! ,lang)
     (with-eval-after-load 'treesit-auto
       (let* (;; 1. built-in treesit-auto recipe for this lang (may be nil)
              (existing (cl-find ',lang treesit-auto-recipe-list
                                 :key #'treesit-auto-recipe-lang))
              ;; 2. caller-supplied args, minus :versions
              (caller-args (list ,@(backpack--plist-remove args :versions)))
              ;; 3. merge: built-in <- caller <- :versions clause
              (merged (backpack--treesit-resolve-recipe
                       (backpack--plist-merge
                        (when existing
                          (backpack--treesit-recipe-to-plist existing))
                        caller-args)
                       ',(plist-get args :versions)))
              (ts-mode    (plist-get merged :ts-mode))
              (remap      (plist-get merged :remap))
              (requires   (plist-get merged :requires))
              (url        (plist-get merged :url))
              (revision   (plist-get merged :revision))
              (source-dir (plist-get merged :source-dir))
              (cc         (plist-get merged :cc))
              (c++        (plist-get merged :c++))
              (ext        (plist-get merged :ext)))
         ;; Remove the existing entry (now captured above) before pushing
         ;; the merged replacement, so there is never more than one entry
         ;; per language in the list.
         (setq treesit-auto-recipe-list
               (seq-remove (lambda (r)
                             (eq (treesit-auto-recipe-lang r) ',lang))
                           treesit-auto-recipe-list))
         (push (apply #'make-treesit-auto-recipe
                      (append
                       (list :lang ',lang)
                       (when ts-mode    (list :ts-mode    ts-mode))
                       (when remap      (list :remap      remap))
                       (when requires   (list :requires   requires))
                       (when url        (list :url        url))
                       (when revision   (list :revision   revision))
                       (when source-dir (list :source-dir source-dir))
                       (when cc         (list :cc         cc))
                       (when c++        (list :c++        c++))
                       (when ext        (list :ext        ext))))
               treesit-auto-recipe-list)))))

(defun backpack--treesit-read-state ()
  "Read the tree-sitter grammar state file.
Returns an alist of (LANG . COMMIT-HASH) or nil if the file does
not exist or cannot be read."
  (when (file-exists-p backpack--treesit-state-file)
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents backpack--treesit-state-file)
          (read (current-buffer)))
      (error nil))))

(defun backpack--treesit-write-state (state)
  "Write STATE alist to the tree-sitter grammar state file.
STATE is an alist of (LANG . COMMIT-HASH)."
  (make-directory (file-name-directory backpack--treesit-state-file) t)
  (with-temp-file backpack--treesit-state-file
    (insert ";; -*- lisp-data -*-\n")
    (insert ";; Tree-sitter grammar revisions tracked by Backpack.\n")
    (insert ";; Auto-generated -- do not edit.\n")
    (prin1 state (current-buffer))
    (insert "\n")))

(defun backpack--treesit-grammar-dir ()
  "Return the directory where tree-sitter grammars are installed.
On Emacs 30+, this is `backpack-tree-sitter-installation-dir'.
On Emacs 29, grammars are installed to the default location
\(the `tree-sitter' subdirectory of `user-emacs-directory') because
`treesit-install-language-grammar' did not accept an OUT-DIR argument
until Emacs 30."
  (if (>= emacs-major-version 30)
      backpack-tree-sitter-installation-dir
    (expand-file-name "tree-sitter" user-emacs-directory)))

(defun backpack--treesit-grammar-so-path (lang)
  "Return the expected shared-library path for tree-sitter LANG.
E.g. for `go' on GNU/Linux this returns
\".cache/nonessentials/tree-sitter/libtree-sitter-go.so\"."
  (let ((soext (or (car dynamic-library-suffixes) ".so")))
    (expand-file-name (concat "libtree-sitter-" (symbol-name lang) soext)
                      (backpack--treesit-grammar-dir))))

(defvar backpack--treesit-pending-sha nil
  "When non-nil, a full Git commit SHA that Backpack wants to check out
after the tree-sitter grammar repository has been cloned.

This is bound dynamically by `backpack--install-treesit-grammars' when
the effective recipe revision is a bare commit hash.  The companion
advice `backpack--treesit-clone-advice' reads this value to inject a
`git checkout' step immediately after the shallow clone that Emacs
normally performs.")

(defun backpack--remove-flag (args flag)
  "Return ARGS with FLAG and its immediately following value removed.
ARGS is a flat list of strings (command-line arguments).  If FLAG
does not appear, ARGS is returned unchanged."
  (let (result)
    (while args
      (let ((a (pop args)))
        (if (equal a flag)
            (pop args)                  ; skip FLAG's value too
          (push a result))))
    (nreverse result)))

(defun backpack--treesit-clone-advice (orig-fun &rest args)
  "Around advice for `treesit--call-process-signal' to handle SHA pinning.

When `backpack--treesit-pending-sha' is non-nil and the call is a
`git clone' invocation, this advice:

  1. Strips the `-b REVISION' flag so Git clones the default branch
     (avoiding the `git clone -b SHA' failure -- -b only accepts named
     refs, not bare commit hashes).
  2. Strips `--depth 1' to perform a full clone instead of a shallow
     one.  A shallow clone only fetches the tip commit, so any earlier
     commit (such as an older pinned SHA) would not be reachable.
  3. After the clone succeeds, runs `git checkout SHA' in the cloned
     working directory to pin the tree to the exact commit before the
     grammar is compiled.

For all other calls (compile steps, other git subcommands, or when no
SHA is pending) the original function is called without modification."
  (if (and backpack--treesit-pending-sha
           ;; program arg is (car args); extra args start at (nthcdr 4 args)
           (equal (car args) "git")
           (member "clone" (nthcdr 4 args)))
      (let* (;; args layout: (program infile buffer display . rest)
             ;; rest for a SHA clone:
             ;;   "clone" url "--depth" "1" "--quiet" "-b" SHA workdir
             (rest    (nthcdr 4 args))
             ;; Remove "-b SHA": git clone -b only accepts branch/tag names.
             ;; Remove "--depth 1": a shallow clone only fetches the tip, so
             ;; older commits (like a pinned SHA) are not reachable.
             (cleaned (backpack--remove-flag
                       (backpack--remove-flag rest "-b")
                       "--depth"))
             (new-args (append (seq-take args 4) cleaned))
             ;; workdir is the last element of the clone argument list
             (workdir (car (last cleaned))))
        ;; Full clone of default branch -- all commits are reachable
        (message "Backpack: Full clone for pinned SHA (no --depth 1)")
        (apply orig-fun new-args)
        ;; Detach HEAD at the pinned commit before compilation
        (message "Backpack: Checking out pinned SHA %s" backpack--treesit-pending-sha)
        (treesit--call-process-signal
         "git" nil t nil "-C" workdir
         "checkout" backpack--treesit-pending-sha))
    (apply orig-fun args)))

;; Install the advice once at load time.  It is a no-op unless
;; `backpack--treesit-pending-sha' is set, so it never interferes with
;; normal (non-Backpack) calls to treesit--call-process-signal.
(with-eval-after-load 'treesit
  (advice-add 'treesit--call-process-signal
              :around #'backpack--treesit-clone-advice))

(defun backpack--treesit-commit-hash-p (revision)
  "Return non-nil if REVISION looks like a full Git commit hash.
A full hash is a string of 40 or more lowercase hexadecimal characters.
`git ls-remote' cannot resolve bare commit hashes (it only lists named
refs such as branches and tags), so callers must handle them specially."
  (and (stringp revision)
       (string-match-p "\\`[0-9a-f]\\{40,\\}\\'" revision)))

(defun backpack--treesit-remote-rev (url &optional revision)
  "Query the remote commit hash for URL at REVISION via git ls-remote.
REVISION is a branch or tag name; when nil the default branch (HEAD)
is queried.  Returns the full commit hash as a string, or nil when
the command fails (e.g. no network).

Note: do not pass a full commit hash as REVISION -- `git ls-remote'
cannot resolve bare SHAs and will return nil.  Use
`backpack--treesit-commit-hash-p' to detect this case beforehand and
short-circuit to the hash itself."
  (condition-case _err
      (with-temp-buffer
        (let ((exit-code
               (call-process "git" nil t nil
                             "ls-remote" url (or revision "HEAD"))))
          (when (eq exit-code 0)
            (goto-char (point-min))
            ;; Output format: "<hash>\t<refname>\n"
            (when (looking-at "\\([0-9a-f]\\{40,\\}\\)")
              (match-string 1)))))
    (error nil)))

(defun backpack--treesit-quarantine-broken-grammar (so-path lang)
  "Best-effort removal of a broken tree-sitter grammar SO-PATH for LANG.

After the post-install ABI check, the grammar shared library is
typically already mapped into the running Emacs process.  On
Windows, mapped libraries cannot be deleted (the OS reports
`permission-denied \"Removing old name\"'), but they can usually
be renamed.  This function tries:

  1. `delete-file' (works on GNU/Linux, macOS, and on Windows when
     the library is not currently mapped).
  2. Otherwise renames SO-PATH to SO-PATH + \".broken\" so that the
     next `backpack ensure' run does not see the broken DLL as
     up-to-date and can clean it up before any treesit operation
     maps it (see `backpack--treesit-cleanup-broken-grammars').

Recoverable errors are swallowed so the caller's outer error message
\(e.g. \"ABI version-mismatch\") is not displaced by a misleading
\"permission-denied\" message."
  (when (file-exists-p so-path)
    (condition-case _err
        (delete-file so-path)
      (file-error
       (let ((quarantine (concat so-path ".broken")))
         (condition-case err2
             (progn
               (when (file-exists-p quarantine)
                 (ignore-errors (delete-file quarantine)))
               (rename-file so-path quarantine t)
               (message "Backpack: Could not delete broken grammar for %s on this OS; renamed to %s.  The next `backpack ensure' will clean it up."
                        lang (file-name-nondirectory quarantine)))
           (file-error
            (message "Backpack: Could not delete or rename broken grammar for %s at %s (%s).  Restart Emacs and re-run `backpack ensure' to recover."
                     lang so-path (error-message-string err2)))))))))

(defun backpack--treesit-cleanup-broken-grammars (dir)
  "Delete any leftover `*.broken' grammar files from previous runs in DIR.
These files are produced by `backpack--treesit-quarantine-broken-grammar'
when the running Emacs process had the freshly-compiled DLL mapped and
could therefore not delete it (Windows behaviour).  A subsequent
`backpack ensure' run starts in a fresh process where nothing is mapped
yet, so the cleanup succeeds."
  (when (file-directory-p dir)
    (dolist (broken (directory-files dir t "\\.broken\\'"))
      (condition-case err
          (progn
            (delete-file broken)
            (message "Backpack: Removed leftover broken grammar %s"
                     (file-name-nondirectory broken)))
        (file-error
         (message "Backpack: Could not remove leftover %s: %s"
                  broken (error-message-string err)))))))

(defun backpack--install-treesit-grammars ()
  "Install all tree-sitter grammars declared by enabled gears.
Only grammars whose upstream revision has changed since the last
successful compilation are rebuilt.  The upstream revision is
determined via `git ls-remote'; if the network is unreachable the
grammar is skipped with a warning.

This should be called during sync mode after all gears are loaded.
Requires treesit-auto to be activated (via `backpack-enable-on-sync!').

When the running Emacs has no tree-sitter support compiled in
\(see `backpack-treesit-available-p') the function short-circuits
with a single informational message and does no work."
  (cond
   ((not (backpack-treesit-available-p))
    (message "Backpack: This Emacs build was not compiled with tree-sitter support; skipping grammar installation.  Rebuild Emacs with `--with-tree-sitter' to enable tree-sitter modes."))
   ((gearp! :ui -treesit)
    nil)
   ((null backpack--treesit-langs)
    nil)
   (t
    (message "Backpack: Checking tree-sitter grammars for: %s"
             (mapconcat #'symbol-name backpack--treesit-langs ", "))

    ;; treesit-auto should already be loaded via backpack-enable-on-sync!
    (if (not (boundp 'treesit-auto-recipe-list))
        (message "Backpack: treesit-auto not available, skipping grammar installation")
      ;; Set treesit-auto-langs to only the languages we need
      (setq treesit-auto-langs backpack--treesit-langs)
      ;; On Emacs 30+, grammars that were previously installed by Emacs 29 into
      ;; <user-emacs-directory>/tree-sitter/ are no longer used (we install to
      ;; backpack-tree-sitter-installation-dir instead).  Rename that legacy
      ;; directory to tree-sitter.orphan so the user knows it can be deleted,
      ;; and so Emacs does not accidentally pick up stale grammars from it.
      (when (>= emacs-major-version 30)
        (let ((legacy-dir (expand-file-name "tree-sitter" user-emacs-directory))
              (orphan-dir (expand-file-name "tree-sitter.orphan" user-emacs-directory)))
          (when (and (file-directory-p legacy-dir)
                     (not (file-exists-p orphan-dir)))
            (message "Backpack: Renaming legacy grammar directory %s -> %s"
                     legacy-dir orphan-dir)
            (rename-file legacy-dir orphan-dir))))
      ;; Set install location.  On Emacs 29, treesit-install-language-grammar
      ;; always installs to the default tree-sitter subdirectory of
      ;; user-emacs-directory; that directory is already searched by Emacs
      ;; without any extra configuration.  On Emacs 30+, we pass our custom
      ;; directory explicitly and keep treesit-extra-load-path pointing at it.
      (let ((grammar-dir (backpack--treesit-grammar-dir)))
        (make-directory grammar-dir t)
        ;; Sweep any *.broken files left by a previous run that hit the
        ;; Windows file-lock path in `backpack--treesit-quarantine-broken-grammar'.
        ;; We are in a fresh emacs process here, so nothing is mapped yet
        ;; and the deletion will succeed.
        (backpack--treesit-cleanup-broken-grammars grammar-dir)
        (when (>= emacs-major-version 30)
          (setq treesit-extra-load-path (list grammar-dir))))
      ;; Build the source alist from treesit-auto recipes
      (let ((treesit-language-source-alist (treesit-auto--build-treesit-source-alist))
            (treesit-auto-install t)  ; Install without prompting
            (state (backpack--treesit-read-state))
            (installed 0)
            (up-to-date 0)
            (failed 0))
        (dolist (lang backpack--treesit-langs)
          (condition-case err
              (let* ((source (alist-get lang treesit-language-source-alist))
                     (url (car source))
                     (revision (cadr source))
                     (so-path (backpack--treesit-grammar-so-path lang))
                     (so-exists (file-exists-p so-path))
                     (prev-rev (alist-get lang state)))
                 (if (not source)
                     (progn
                       (message "Backpack: No recipe found for %s, skipping" lang)
                       (cl-incf failed))
                   ;; Determine the effective remote revision.
                   ;; When the recipe pins a full commit hash we use it
                   ;; directly: git ls-remote cannot resolve bare SHAs
                   ;; (it only lists named refs) and would return nil,
                   ;; which we would otherwise misinterpret as a network
                   ;; error.  A pinned hash is inherently stable -- the
                   ;; same SHA always points to the same commit.
                   (let ((remote-rev
                          (if (backpack--treesit-commit-hash-p revision)
                              revision
                            (backpack--treesit-remote-rev url revision))))
                     (cond
                      ;; Network failure -- cannot determine state
                      ((null remote-rev)
                       (message "Backpack: WARNING: Cannot check updates for %s (network error?), skipping" lang)
                       (cl-incf failed))
                     ;; Already compiled and up to date
                     ((and so-exists prev-rev (string= remote-rev prev-rev))
                      (message "Backpack: Grammar for %s is up to date, skipping" lang)
                      (cl-incf up-to-date))
                      ;; Needs (re)compilation
                      (t
                       (message "Backpack: Installing grammar for %s..." lang)
                       ;; When the revision is a bare commit hash we must not
                       ;; pass it as-is to treesit-install-language-grammar:
                       ;; Emacs always uses the revision as the -b argument to
                       ;; `git clone', which only accepts branch/tag names.
                       ;;
                       ;; Strategy:
                       ;;   1. Temporarily replace the revision in
                       ;;      treesit-language-source-alist with nil so that
                       ;;      Emacs clones the default branch (no -b flag).
                       ;;   2. Bind backpack--treesit-pending-sha to the SHA.
                       ;;      The advice backpack--treesit-clone-advice on
                       ;;      treesit--call-process-signal detects this and
                       ;;      injects a `git checkout SHA' step after the
                       ;;      clone succeeds, pinning the tree to the exact
                       ;;      commit before compilation.
                       (let* ((sha-p (backpack--treesit-commit-hash-p revision))
                              (backpack--treesit-pending-sha (when sha-p revision))
                              ;; Shadow the source alist: replace SHA revision
                              ;; with nil so treesit clones the default branch.
                              (treesit-language-source-alist
                               (if sha-p
                                   (cons (cons lang
                                               (list url nil
                                                     (nth 2 source)
                                                     (nth 3 source)
                                                     (nth 4 source)))
                                         (assoc-delete-all
                                          lang treesit-language-source-alist))
                                 treesit-language-source-alist)))
                         ;; Emacs 29: treesit-install-language-grammar only
                         ;; accepts one argument; grammars land in the default
                         ;; location (<user-emacs-directory>/tree-sitter/).
                         ;; Emacs 30+: pass the custom directory explicitly.
                         (if (>= emacs-major-version 30)
                             (treesit-install-language-grammar
                              lang backpack-tree-sitter-installation-dir)
                           (treesit-install-language-grammar lang)))
                      ;; Verify the installed grammar is actually loadable.
                      ;; A version-mismatch means the grammar was compiled
                      ;; against a newer ABI than this Emacs binary supports.
                      (pcase (treesit-language-available-p lang t)
                        (`(t . ,_)
                         (setf (alist-get lang state) remote-rev)
                         (cl-incf installed))
                        (`(nil . (version-mismatch . ,abi))
                         (message "Backpack: Grammar for %s has ABI version-mismatch (compiled to ABI %s, this Emacs supports up to ABI %s); marking as failed.  Pin an older revision via `:versions ((:until-abi %s :revision \"...\"))' or upgrade libtree-sitter."
                                  lang abi
                                  (and (fboundp 'treesit-library-abi-version)
                                       (treesit-library-abi-version))
                                  (and (fboundp 'treesit-library-abi-version)
                                       (treesit-library-abi-version)))
                         ;; Best-effort cleanup of the unusable library so
                         ;; the next ensure run does not treat it as
                         ;; up-to-date.  On Windows the freshly-compiled
                         ;; DLL is now mapped into this process and cannot
                         ;; be deleted; quarantine handles that case.
                         (backpack--treesit-quarantine-broken-grammar so-path lang)
                         (cl-incf failed))
                        (`(nil . ,err)
                         (message "Backpack: Grammar for %s failed post-install check (%s), marking as failed" lang err)
                         (backpack--treesit-quarantine-broken-grammar so-path lang)
                         (cl-incf failed))))))))
            (error
             (message "Backpack: Failed to install grammar for %s: %s" lang err)
             (cl-incf failed))))
        ;; Persist state so the next run can skip unchanged grammars
        (backpack--treesit-write-state state)
        (message "Backpack: Tree-sitter grammars: %d installed, %d up-to-date, %d failed/skipped"
                 installed up-to-date failed))))))


(defun backpack-treesit--buffer-language ()
  "Return the tree-sitter language symbol for the current buffer, or nil."
  (or (when (and (fboundp 'treesit-parser-list)
                 (treesit-parser-list))
        (treesit-parser-language (car (treesit-parser-list))))
      (when (fboundp 'treesit-language-at)
        (treesit-language-at (point)))
      (when (string-suffix-p "-ts-mode" (symbol-name major-mode))
        (intern (substring (symbol-name major-mode) 0
                           (- (length (symbol-name major-mode))
                              (length "-ts-mode")))))))

(defun backpack-treesit--short-hash (hash)
  "Return the first 7 characters of HASH, or HASH if shorter."
  (if (and (stringp hash) (> (length hash) 7))
      (substring hash 0 7)
    hash))

(defun backpack-treesit--state-commit (lang)
  "Return the installed commit hash for LANG from the Backpack state file, or nil."
  (when (and (boundp 'backpack--treesit-state-file)
             (fboundp 'backpack--treesit-read-state))
    (alist-get lang (backpack--treesit-read-state))))

(defun backpack-treesit--recipe-info (lang)
  "Return a plist of (:url URL :revision REV :commit HASH) for LANG from `treesit-language-source-alist', or nil."
  (let ((entry (assoc lang treesit-language-source-alist)))
    (when entry
      (let ((val (cdr entry))
            result)
        (when val
          (if (stringp val)
              (setq result (plist-put result :url val))
            (when (and val (not (keywordp (car val))))
              (setq result (plist-put result :url (car val)))
              (setq val (cdr val)))
            (while val
              (let ((k (pop val))
                    (v (pop val)))
                (when (and (keywordp k) v)
                  (setq result
                        (plist-put result
                                   (cl-case k
                                     (:revision :revision)
                                     (:commit :commit)
                                     (:url :url)
                                     (otherwise k))
                                   v))))))
          result)))))

;;;###autoload
(defun backpack-treesit-grammar-info ()
  "Display tree-sitter grammar info for the current buffer's language.
On Emacs builds without tree-sitter support compiled in, this
command reports that condition instead of trying to inspect a
non-existent grammar."
  (interactive)
  (if (not (backpack-treesit-available-p))
      (message "Backpack: This Emacs build was not compiled with tree-sitter support.")
  (let* ((lang (backpack-treesit--buffer-language))
         (propertize (lambda (str) (propertize str 'face 'font-lock-type-face))))
    (if (not lang)
        (message "No tree-sitter language in current buffer")
      (let ((abi (when (fboundp 'treesit-language-abi-version)
                   (treesit-language-abi-version lang)))
            (lib-max (when (fboundp 'treesit-library-abi-version)
                       (treesit-library-abi-version)))
            (lib-min (when (fboundp 'treesit-library-abi-version)
                       (treesit-library-abi-version t)))
            (path (when (fboundp 'treesit-grammar-location)
                    (treesit-grammar-location lang)))
            (commit (backpack-treesit--state-commit lang))
            (short-lang (symbol-name lang)))
        (if (not abi)
            (message "%s: grammar not installed" (funcall propertize short-lang))
          (let* ((home-dir (expand-file-name "~/"))
                 (short-path (if (and path (string-prefix-p home-dir path))
                                 (concat "~/" (substring path (length home-dir)))
                               path))
                 (abi-str (format "ABI %s" abi))
                 (lib-range (if (and lib-min lib-max)
                                (format " (lib %s–%s)" lib-min lib-max)
                              ""))
                 (commit-str (if commit
                                (format ", commit %s"
                                        (backpack-treesit--short-hash commit))
                              "")))
            (message "%s: %s%s, %s%s"
                     (funcall propertize short-lang)
                     abi-str lib-range
                     (or short-path "path unknown")
                     commit-str))))))))

(provide 'backpack-treesit)
;;; backpack-treesit.el ends here
