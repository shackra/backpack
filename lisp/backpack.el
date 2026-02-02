;; -*- lexical-binding: t; -*-

(eval-and-compile
  (when (version< emacs-version "29.1")
    (error "Backpack is only compatible with Emacs version 29.1 and up")))

(let ((old-version (eval-when-compile emacs-version)))
  (unless (string= emacs-version old-version)
    (user-error (format "Backpack was compiled with Emacs %s, but was loaded with %s" emacs-version old-version))))

;; Copy the consistency imposed by Doom Emacs
(when (bound-and-true-p module-file-suffix)
  (push 'dynamic-modules features))
(when (fboundp #'json-parse-string)
  (push 'jansson features))
(when (string-match-p "HARFBUZZ" system-configuration-features)
  (push 'harfbuzz features))

;; don't bother with native compilation if it's not functional
(when (and (featurep 'native-compile)
	   (not (native-comp-available-p)))
  (delq 'native-compile features))

;; don't worry the user about obsolete macros
(put 'if-let 'byte-obsolete-info nil)
(put 'when-let 'byte-obsolete-info nil)

;; backpack standard library
(add-to-list 'load-path (expand-file-name "base-packages/leaf.el" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "base-packages/leaf-keywords.el" user-emacs-directory))
(add-to-list 'load-path (file-name-directory load-file-name))
(require 'leaf)
(require 'leaf-keywords)
(require 'backpack-pouch)
(require 'backpack-email-utils)

(leaf-keywords-init)

;;; add additional keywords to leaf block
;; :doctor defines binaries to check on the user's system
;; :fonts check what if the fonts needed by a package are installed
(plist-put leaf-keywords :doctor '`(,@leaf--body))
(plist-put leaf-keywords :fonts '`(,@leaf--body))

;; alias :ensure to :elpaca
(setq leaf-alias-keyword-alist '((:ensure . :elpaca)))

;;; Backpack mode management
;; NOTE: This must be defined BEFORE the leaf advice below
(defvar backpack-mode 'normal
  "Current operating mode for Backpack.
Possible values:
- `normal': Standard Emacs startup - only activate pre-built packages
- `sync': Synchronization mode - install/build packages but skip activation")

(defun backpack-sync-mode-p ()
  "Return non-nil if Backpack is in synchronization mode."
  (eq backpack-mode 'sync))

(defun backpack-normal-mode-p ()
  "Return non-nil if Backpack is in normal mode."
  (eq backpack-mode 'normal))

;; NOTE: We previously tried to advise the `leaf` macro to filter keywords in sync mode,
;; but `leaf` is a macro, not a function, so `:around` advice doesn't work properly.
;; Instead, we rely on the elpaca advice (backpack--elpaca-skip-forms-in-sync-mode)
;; to prevent configuration forms from running during sync mode.

;;; Tree-sitter language management
(defvar backpack--treesit-langs nil
  "List of tree-sitter language symbols that are needed by enabled gears.
This is populated by `backpack-treesit-langs!' calls in gear files.")

(defmacro backpack-treesit-langs! (&rest langs)
  "Declare that the current gear needs tree-sitter support for LANGS.
LANGS should be symbols like `go', `python', `json', etc.
These will be added to `treesit-auto-langs' and installed during sync."
  `(dolist (lang ',langs)
     (cl-pushnew lang backpack--treesit-langs)))

(defun backpack--install-treesit-grammars ()
  "Install all tree-sitter grammars declared by enabled gears.
This should be called during sync mode after all gears are loaded."
  (when (and backpack--treesit-langs
             (not (gearp! :ui -treesit)))
    (message "Backpack: Installing tree-sitter grammars for: %s"
             (mapconcat #'symbol-name backpack--treesit-langs ", "))

    ;; In sync mode, treesit-auto is installed but not activated.
    ;; We need to manually add its build directory to load-path and load it.
    (let ((treesit-auto-build-dir (expand-file-name "treesit-auto" elpaca-builds-directory)))
      (when (file-exists-p treesit-auto-build-dir)
        (add-to-list 'load-path treesit-auto-build-dir)
        ;; Load the autoloads first
        (let ((autoloads (expand-file-name "treesit-auto-autoloads.el" treesit-auto-build-dir)))
          (when (file-exists-p autoloads)
            (load autoloads nil t)))))

    ;; Now require treesit-auto
    (require 'treesit-auto nil t)

    (if (not (fboundp 'treesit-auto-recipe-alist))
        (message "Backpack: treesit-auto not available, skipping grammar installation")
      ;; Set treesit-auto-langs to only the languages we need
      (setq treesit-auto-langs backpack--treesit-langs)
      ;; Set install location
      (setq treesit-extra-load-path (list backpack-tree-sitter-installation-dir))
      ;; Make sure the installation directory exists
      (make-directory backpack-tree-sitter-installation-dir t)
      ;; Install without prompting
      (let ((treesit-auto-install t)
            (installed 0)
            (failed 0))
        (dolist (lang backpack--treesit-langs)
          (condition-case err
              (let ((recipe (alist-get lang treesit-auto-recipe-alist)))
                (if (not recipe)
                    (progn
                      (message "Backpack: No recipe found for %s, skipping" lang)
                      (cl-incf failed))
                  (message "Backpack: Installing grammar for %s..." lang)
                  (treesit-install-language-grammar lang backpack-tree-sitter-installation-dir)
                  (cl-incf installed)))
            (error
             (message "Backpack: Failed to install grammar for %s: %s" lang err)
             (cl-incf failed))))
        (message "Backpack: Tree-sitter grammars: %d installed, %d failed/skipped"
                 installed failed)))))

(defconst backpack-system
  (pcase system-type
    ('darwin '(macos bsd))
    ((or 'cygwin 'windows-nt 'ms-dos) '(windows))
    ((or 'gnu 'gnu/linux) '(gnu))
    ((or 'gnu/kfreebsd 'berkeley-unix) '(gnu bsd))
    ('android '(android)))
  "The operating system Emacs is running on.")

(defconst backpack--system-windows-p (eq 'windows (car backpack-system)))
(defconst backpack--system-macos-p (eq 'macos (car backpack-system)))
(defconst backpack--system-gnu-p (eq 'gnu (car backpack-system)))

(when (and backpack--system-gnu-p
	   (getenv-internal "WSLENV"))
  (add-to-list backpack-system 'wsl 'append))

(push :system features)
(put :system 'subfeatures backpack-system)

(when backpack--system-windows-p
  (when-let* ((realhome
	       (and (null (getenv-internal "HOME"))
		    (getenv "USERPROFILE"))))
    (setenv "HOME" realhome)
    (setq abbreviated-home-dir nil)))

;;; Globals

(defgroup backpack nil
  "A self-documenting GNU Emacs starter kit inspired after Bedrock and Doom."
  :link '(url-link "https://github.com/shackra/backpack"))

(defvar backpack-emacs-dir user-emacs-directory
  "The path of the currently loaded Emacs configuration.")

(defconst backpack-core-dir (file-name-directory load-file-name)
  "The directory of Backpack files.")

(defvar backpack-user-dir
  (let ((in-xdg-config (expand-file-name "backpack/" (getenv-internal "XDG_CONFIG_HOME")))
	(in-user-home "~/.backpack.d/"))
    (if (file-exists-p in-user-home)
	in-user-home
      in-xdg-config))
  "Location of the user's private configuration.

Either ~/.config/backpack or ~/.backpack.d/.")

(defvar backpack-cache-dir (expand-file-name ".cache/" backpack-emacs-dir)
  "Location for local storage")

(defvar backpack-data-dir
  (expand-file-name "etc" backpack-cache-dir)
  "Location use by Backpack to store important files.

Delete this directory entails user intervention to make things work
again.")

(defvar backpack-nonessential-dir
  (expand-file-name "nonessentials" backpack-cache-dir)
  "Location where Backpack stores nonessential files.

If anything is missing here, Backpack Emacs will work as normal.")

(defvar backpack-state-dir
  (expand-file-name "state" backpack-cache-dir)
  "Location for files that carry state for some functionalities or packages.")

(defvar backpack-tree-sitter-installation-dir
  (expand-file-name "tree-sitter" backpack-nonessential-dir)
  "Location for treesit to install compiled grammar.")

;;
;;; Startup optimizations
;;; copied straight from Doom Emacs 👀

(unless (daemonp)
  ;; PERF: `file-name-handler-alist' is consulted on each call to `require',
  ;;   `load', or various file/io functions (like `expand-file-name' or
  ;;   `file-remote-p'). You get a noteable boost to startup time by unsetting
  ;;   or simplifying its value.
  (let ((old-value (default-toplevel-value 'file-name-handler-alist)))
    (set-default-toplevel-value
     'file-name-handler-alist
     ;; HACK: The elisp libraries bundled with Emacs are either compressed or
     ;;   not, never both. So if calc-loaddefs.el.gz exists, calc-loaddefs.el
     ;;   won't, and vice versa. This heuristic is used to guess the state of
     ;;   all other built-in (or site); if they're compressed, we must leave the
     ;;   gzip file handler in `file-name-handler-alist' so Emacs knows how to
     ;;   load them. Otherwise, we can omit it (at least during startup) for a
     ;;   boost in package load time.
     (if (eval-when-compile
           (locate-file-internal "calc-loaddefs.el" load-path))
         nil
       (list (rassq 'jka-compr-handler old-value))))
    ;; Remember it so it can be reset where needed.
    (put 'file-name-handler-alist 'initial-value old-value)
    ;; COMPAT: Eventually, Emacs will process any files passed to it via the
    ;;   command line, and will do so *really* early in the startup process.
    ;;   These might contain special file paths like TRAMP paths, so restore
    ;;   `file-name-handler-alist' just for this portion of startup.
    (define-advice command-line-1 (:around (fn args-left) respect-file-handlers)
      (let ((file-name-handler-alist (if args-left old-value file-name-handler-alist)))
        (funcall fn args-left)))
    ;; COMPAT: ...but restore `file-name-handler-alist' later, because it is
    ;;   needed for handling encrypted or compressed files, among other things.
    (add-hook 'emacs-startup-hook
	      (defun backpack--reset-file-handler-alist-h ()
		(set-default-toplevel-value
		 'file-name-handler-alist
		 ;; Merge instead of overwrite because there may have been changes to
		 ;; `file-name-handler-alist' since startup we want to preserve.
		 (delete-dups (append file-name-handler-alist old-value))))
	      100))

  (unless noninteractive
    ;; PERF: Resizing the Emacs frame (to accommodate fonts that are smaller or
    ;;   larger than the default system font) can impact startup time
    ;;   dramatically. The larger the delta, the greater the delay. Even trivial
    ;;   deltas can yield up to a ~1000ms loss, depending also on
    ;;   `window-system' (PGTK builds seem least affected and NS/MAC the most).
    (setq frame-inhibit-implied-resize t)

    ;; PERF: A fair bit of startup time goes into initializing the splash and
    ;;   scratch buffers in the typical Emacs session (b/c they activate a
    ;;   non-trivial major mode, generate the splash buffer, and trigger
    ;;   premature frame redraws by writing to *Messages*). These hacks prevent
    ;;   most of this work from happening for some decent savings in startup
    ;;   time. Our dashboard and `doom/open-scratch-buffer' provide a faster
    ;;   (and more useful) alternative anyway.
    (setq inhibit-startup-screen t
          inhibit-startup-echo-area-message user-login-name
          initial-major-mode 'fundamental-mode
          initial-scratch-message nil)
    ;; PERF,UX: Prevent "For information about GNU Emacs..." line in *Messages*.
    (advice-add #'display-startup-echo-area-message :override #'ignore)
    ;; PERF: Suppress the vanilla startup screen completely. We've disabled it
    ;;   with `inhibit-startup-screen', but it would still initialize anyway.
    ;;   This involves file IO and/or bitmap work (depending on the frame type)
    ;;   that we can no-op for a free 50-100ms saving in startup time.
    (advice-add #'display-startup-screen :override #'ignore)

    (unless initial-window-system
      ;; PERF: `tty-run-terminal-initialization' can take 2-3s when starting up
      ;;   TTY Emacs (non-daemon sessions), depending on your TERM, TERMINFO,
      ;;   and TERMCAP, but this work isn't very useful on modern systems (the
      ;;   type I expect Doom's users to be using). The function seems less
      ;;   expensive if run later in the startup process, so I defer it.
      ;; REVIEW: This may no longer be needed in 29+. Needs testing!
      (define-advice tty-run-terminal-initialization (:override (&rest _) defer)
        (advice-remove #'tty-run-terminal-initialization #'tty-run-terminal-initialization@defer)
        (add-hook 'window-setup-hook
                  (apply-partially #'tty-run-terminal-initialization
                                   (selected-frame) nil t))))

    ;; These optimizations are brittle, difficult to debug, and obscure other
    ;; issues, so bow out when debug mode is on.
    (unless init-file-debug
      ;; PERF: The mode-line procs a couple dozen times during startup, before
      ;;   the user even sees the first mode-line. This is normally fast, but we
      ;;   can't predict what the user (or packages) will put into the
      ;;   mode-line. Also, mode-line packages have a bad habit of throwing
      ;;   performance to the wind, so best we just disable the mode-line until
      ;;   we can see one.
      (put 'mode-line-format 'initial-value (default-toplevel-value 'mode-line-format))
      (setq-default mode-line-format nil)
      (dolist (buf (buffer-list))
        (with-current-buffer buf (setq mode-line-format nil)))
      ;; PERF,UX: Premature redisplays/redraws can substantially affect startup
      ;;   times and/or flash a white/unstyled Emacs frame during startup, so I
      ;;   try real hard to suppress them until we're sure the session is ready.
      (setq-default inhibit-redisplay t
                    inhibit-message t)
      ;; COMPAT: If the above vars aren't reset, Emacs could appear frozen or
      ;;   garbled after startup (or in case of an startup error).
      (defun backpack--reset-inhibited-vars-h ()
        (setq-default inhibit-redisplay nil
                      inhibit-message nil)
        (remove-hook 'post-command-hook #'backpack--reset-inhibited-vars-h))
      (add-hook 'post-command-hook #'backpack--reset-inhibited-vars-h -100))

    ;; PERF: Doom disables the UI elements by default, so that there's less for
    ;;   the frame to initialize. However, `tool-bar-setup' is still called and
    ;;   it does some non-trivial work to set up the toolbar before we can
    ;;   disable it. To side-step this work, I disable the function and call it
    ;;   later (see `startup--load-user-init-file@undo-hacks').
    (advice-add #'tool-bar-setup :override #'ignore)

    ;; PERF,UX: site-lisp files are often obnoxiously noisy (emitting output
    ;;   that isn't useful to end-users, like load messages, deprecation
    ;;   notices, and linter warnings). Displaying these in the minibuffer
    ;;   causes unnecessary redraws at startup which can impact startup time
    ;;   drastically and cause flashes of white. It also pollutes the logs. I
    ;;   suppress it here and load it myself, later, in a more controlled way
    ;;   (see `doom-initialize').
    (put 'site-run-file 'initial-value site-run-file)
    (setq site-run-file nil)

    (define-advice startup--load-user-init-file (:around (fn &rest args) undo-hacks 95)
      "Undo Doom's startup optimizations to prep for the user's session."
      (unwind-protect (apply fn args)
        ;; Now it's safe to be verbose.
        (setq-default inhibit-message nil)
        ;; COMPAT: Once startup is sufficiently complete, undo our earlier
        ;;   optimizations to reduce the scope of potential edge cases.
        (advice-remove #'tool-bar-setup #'ignore)

        (add-hook 'tool-bar-mode-hook (defun --tool-bar-setup ()
					(tool-bar-setup)
					(remove-hook 'tool-bar-mode-hook '--tool-bar-setup)))
        (unless (default-toplevel-value 'mode-line-format)
          (setq-default mode-line-format (get 'mode-line-format 'initial-value)))))

    ;; PERF: Unset a non-trivial list of command line options that aren't
    ;;   relevant to this session, but `command-line-1' still processes.
    (unless backpack--system-macos-p
      (setq command-line-ns-option-alist nil))
    (unless (memq initial-window-system '(x pgtk))
      (setq command-line-x-option-alist nil))))

;;
;;; Reasonable, global defaults

;;; CLI settings
(when noninteractive
  ;; Don't generate superfluous files when writing temp buffers.
  (setq make-backup-files nil)
  ;; Stop user config from interfering with elisp shell scripts.
  (setq enable-dir-local-variables nil)
  ;; Reduce ambiguity, embrace specificity, enjoy predictability.
  (setq case-fold-search nil)
  ;; Don't clog the user's trash with our CLI refuse.
  (setq delete-by-moving-to-trash nil))

;;; Don't litter `doom-emacs-dir'/$HOME
;; HACK: I change `user-emacs-directory' because many packages (even built-in
;;   ones) abuse it to build paths for storage/cache files (instead of correctly
;;   using `locate-user-emacs-file'). This change ensures that said data files
;;   are never saved to the root of your emacs directory *and* saves us the
;;   trouble of setting a million directory/file variables.
(setq user-emacs-directory backpack-cache-dir)

;; ...However, this may surprise packages (and users) that read
;; `user-emacs-directory' expecting to find the location of your Emacs config,
;; such as server.el!
(setq server-auth-dir (file-name-concat backpack-emacs-dir "server/"))

;; Packages with file/dir settings that don't use `user-emacs-directory' or
;; `locate-user-emacs-file' to initialize will need to set explicitly, to stop
;; them from littering in ~/.emacs.d/.
(setq desktop-dirname  (file-name-concat backpack-state-dir "desktop")
      pcache-directory (file-name-concat backpack-cache-dir "pcache/"))

;; Allow the user to store custom.el-saved settings and themes in their Doom
;; config (e.g. ~/.doom.d/).
(setq custom-file (file-name-concat backpack-user-dir "custom.el"))

;; backup all files here
(setq backup-directory-alist `(("." . ,backpack-nonessential-dir)))

(define-advice en/disable-command (:around (fn &rest args) write-to-data-dir)
  "Save safe-local-variables to `custom-file' instead of `user-init-file'.

Otherwise, `en/disable-command' (in novice.el.gz) is hardcoded to write them to
`user-init-file')."
  (let ((user-init-file custom-file))
    (apply fn args)))

;;; Native compilation support (see http://akrl.sdf.org/gccemacs.html)
(when (boundp 'native-comp-eln-load-path)
  ;; Don't store eln files in ~/.emacs.d/eln-cache (where they can easily be
  ;; deleted by 'doom upgrade').
  ;; REVIEW: Advise `startup-redirect-eln-cache' when 28 support is dropped.
  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln/" backpack-cache-dir))

  ;; UX: Suppress compiler warnings and don't inundate users with their popups.
  ;;   They are rarely more than warnings, so are safe to ignore.
  (setq native-comp-async-report-warnings-errors init-file-debug
        native-comp-warning-on-missing-source init-file-debug)

  ;; HACK: `native-comp-deferred-compilation-deny-list' is replaced in later
  ;;   versions of Emacs 29, and with no deprecation warning. I alias them to
  ;;   ensure backwards compatibility for packages downstream that may have not
  ;;   caught up yet. I avoid marking it obsolete because obsolete warnings are
  ;;   unimportant to end-users. It's the package devs that should be informed.
  (unless (boundp 'native-comp-deferred-compilation-deny-list)
    (defvaralias 'native-comp-deferred-compilation-deny-list 'native-comp-jit-compilation-deny-list))

  ;; UX: By default, native-comp uses 100% of half your cores. If you're
  ;;   expecting this this should be no issue, but the sudden (and silent) spike
  ;;   of CPU and memory utilization can alarm folks, overheat laptops, or
  ;;   overwhelm less performant systems.
  (define-advice comp-effective-async-max-jobs (:before (&rest _) set-default-cpus)
    "Default to 1/4 of cores in interactive sessions and all of them otherwise."
    (and (null comp-num-cpus)
         (zerop native-comp-async-jobs-number)
         (setq comp-num-cpus
               (max 1 (/ (num-processors) (if noninteractive 1 4))))))

  (define-advice comp-run-async-workers (:around (fn &rest args) dont-litter-tmpdir)
    "Normally, native-comp writes a ton to /tmp. This advice forces it to write
to `doom-profile-cache-dir' instead, so it can be safely cleaned up as part of
'doom sync' or 'doom gc'."
    (let ((temporary-file-directory (expand-file-name "comp/" backpack-cache-dir)))
      (make-directory temporary-file-directory t)
      (apply fn args)))

  (with-eval-after-load 'comp
    ;; HACK: On Emacs 30.0.92, `native-comp-jit-compilation-deny-list' was moved
    ;;   to comp-run. See emacsmirror/emacs@e6a955d24268. Doom forces straight
    ;;   to consult this variable when building packages.
    (require 'comp-run nil t)
    ;; HACK: Disable native-compilation for some troublesome packages
    (mapc (apply-partially #'add-to-list 'native-comp-deferred-compilation-deny-list)
          (list "/seq-tests\\.el\\'"
                "/emacs-jupyter.*\\.el\\'"
                "/evil-collection-vterm\\.el\\'"
                "/vterm\\.el\\'"
                "/with-editor\\.el\\'"))))


;;; Reduce unnecessary/unactionable warnings/logs
;; Disable warnings from the legacy advice API. They aren't actionable or
;; useful, and often come from third party packages.
(setq ad-redefinition-action 'accept)

;; Ignore warnings about "existing variables being aliased". Otherwise the user
;; gets very intrusive popup warnings about our (intentional) uses of
;; defvaralias, which are done because ensuring aliases are created before
;; packages are loaded is an unneeded and unhelpful maintenance burden. Emacs
;; still aliases them fine regardless.
(setq warning-suppress-types '((defvaralias) (lexical-binding)))

;; As some point in 31+, Emacs began spamming the user with warnings about
;; missing `lexical-binding' cookies in elisp files that you are unlikely to
;; have any direct control over (e.g. package files, data lisp files, and elisp
;; shell scripts). This shuts it up.
(setq warning-inhibit-types '((files missing-lexbind-cookie)))

;; Reduce debug output unless we've asked for it.
(setq debug-on-error init-file-debug
      jka-compr-verbose init-file-debug)

;;; Stricter security defaults
;; Emacs is essentially one huge security vulnerability, what with all the
;; dependencies it pulls in from all corners of the globe. Let's try to be a
;; *little* more discerning.
(setq gnutls-verify-error noninteractive
      gnutls-algorithm-priority
      (when (boundp 'libgnutls-version)
        (concat "SECURE128:+SECURE192:-VERS-ALL"
                (if (and (not backpack--system-windows-p)
                         (>= libgnutls-version 30605))
                    ":+VERS-TLS1.3")
                ":+VERS-TLS1.2"))
      ;; `gnutls-min-prime-bits' is set based on recommendations from
      ;; https://www.keylength.com/en/4/
      gnutls-min-prime-bits 3072
      tls-checktrust gnutls-verify-error
      ;; Emacs is built with gnutls.el by default, so `tls-program' won't
      ;; typically be used, but in the odd case that it does, we ensure a more
      ;; secure default for it (falling back to `openssl' if absolutely
      ;; necessary). See https://redd.it/8sykl1 for details.
      tls-program '("openssl s_client -connect %h:%p -CAfile %t -nbio -no_ssl3 -no_tls1 -no_tls1_1 -ign_eof"
                    "gnutls-cli -p %p --dh-bits=3072 --ocsp --x509cafile=%t \
--strict-tofu --priority='SECURE192:+SECURE128:-VERS-ALL:+VERS-TLS1.2:+VERS-TLS1.3' %h"
                    ;; compatibility fallbacks
                    "gnutls-cli -p %p %h"))


;;; Package managers
;; Since Emacs 27, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. Doom handles package initialization, so
;; we must prevent Emacs from doing it again.
(setq package-enable-at-startup nil)

;;
;;; Initializers

(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" backpack-nonessential-dir))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))

;; Location of elpaca in base-packages (bundled with Backpack)
(defvar backpack--elpaca-source-dir
  (expand-file-name "base-packages/elpaca/" backpack-emacs-dir)
  "Location of the bundled elpaca source in base-packages.")

;; Custom build steps for synchronization mode - everything except activation
(defvar backpack--sync-build-steps
  '(elpaca--clone
    elpaca--configure-remotes
    elpaca--checkout-ref
    elpaca--run-pre-build-commands
    elpaca--queue-dependencies
    elpaca--check-version
    elpaca--link-build-files
    elpaca--generate-autoloads-async
    elpaca--byte-compile
    elpaca--compile-info
    elpaca--install-info
    elpaca--add-info-path
    elpaca--run-post-build-commands)
  "Build steps for sync mode - excludes `elpaca--activate-package'.")

;; Steps for activating pre-built packages in normal mode
(defvar backpack--activation-only-steps
  '(elpaca--queue-dependencies elpaca--add-info-path elpaca--activate-package)
  "Steps for normal mode - only activate already-built packages.")

(defun backpack--copy-directory-recursively (source dest)
  "Copy SOURCE directory recursively to DEST using pure Emacs Lisp.
This is platform-agnostic and doesn't rely on external tools."
  (unless (file-exists-p dest)
    (make-directory dest t))
  (dolist (file (directory-files source t "^[^.]"))
    (let ((dest-file (expand-file-name (file-name-nondirectory file) dest)))
      (cond
       ((file-directory-p file)
        (backpack--copy-directory-recursively file dest-file))
       (t
        (copy-file file dest-file t))))))

(defun backpack--elpaca-repo-dir ()
  "Return the elpaca repo directory path."
  (expand-file-name "elpaca/" elpaca-repos-directory))

(defun backpack--elpaca-build-dir ()
  "Return the elpaca build directory path."
  (expand-file-name "elpaca/" elpaca-builds-directory))

(defun backpack--elpaca-installed-p ()
  "Return non-nil if elpaca is already installed in the expected location."
  (let ((repo-dir (backpack--elpaca-repo-dir)))
    (and (file-exists-p repo-dir)
         (file-exists-p (expand-file-name "elpaca.el" repo-dir)))))

(defun backpack--install-elpaca-from-base-packages ()
  "Copy elpaca from base-packages to elpaca-repos-directory.
This uses the bundled elpaca instead of cloning from the internet."
  (let ((repo-dir (backpack--elpaca-repo-dir)))
    (message "Backpack: Installing elpaca from base-packages...")
    ;; Ensure parent directories exist
    (make-directory elpaca-repos-directory t)
    ;; Copy elpaca source to repo directory
    (backpack--copy-directory-recursively backpack--elpaca-source-dir repo-dir)
    (message "Backpack: Elpaca source copied to %s" repo-dir)))

(defun backpack--build-elpaca ()
  "Build elpaca: byte-compile and generate autoloads.
This replicates what the elpaca installer does but without cloning."
  (let* ((repo-dir (backpack--elpaca-repo-dir))
         (build-dir (backpack--elpaca-build-dir))
         (default-directory repo-dir))
    (message "Backpack: Building elpaca...")

    ;; Byte-compile elpaca
    (let ((emacs-exe (concat invocation-directory invocation-name)))
      (call-process emacs-exe nil nil nil
                    "-Q" "-L" "." "--batch"
                    "--eval" "(byte-recompile-directory \".\" 0 'force)"))

    ;; Load elpaca to generate autoloads
    (add-to-list 'load-path repo-dir)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo-dir)

    ;; Create build directory with symlinks/copies to repo
    ;; Link all .el and .elc files including autoloads
    (make-directory build-dir t)
    (dolist (file (directory-files repo-dir t "\\.elc?\\'"))
      (let ((dest (expand-file-name (file-name-nondirectory file) build-dir)))
        (unless (file-exists-p dest)
          (if (fboundp 'make-symbolic-link)
              (condition-case nil
                  (make-symbolic-link file dest)
                (error (copy-file file dest t)))
            (copy-file file dest t)))))

    (message "Backpack: Elpaca built successfully")))

(defun backpack--ensure-elpaca ()
  "Ensure elpaca is installed and built from base-packages.
In sync mode, this will copy and build elpaca if needed.
In normal mode, this just loads elpaca if it's already built."
  (let ((repo-dir (backpack--elpaca-repo-dir))
        (build-dir (backpack--elpaca-build-dir)))

    (cond
     ;; Elpaca is already built - just load it
     ((and (file-exists-p build-dir)
           (file-exists-p (expand-file-name "elpaca-autoloads.el" build-dir)))
      (add-to-list 'load-path build-dir)
      (require 'elpaca-autoloads nil t))

     ;; Elpaca source exists but not built - build it (sync mode)
     ((and (backpack-sync-mode-p)
           (backpack--elpaca-installed-p))
      (backpack--build-elpaca)
      (add-to-list 'load-path build-dir)
      (require 'elpaca-autoloads nil t))

     ;; Elpaca not installed - install from base-packages (sync mode only)
     ((backpack-sync-mode-p)
      (backpack--install-elpaca-from-base-packages)
      (backpack--build-elpaca)
      (add-to-list 'load-path build-dir)
      (require 'elpaca-autoloads nil t))

     ;; Normal mode but elpaca not installed - try to load from repo
     ((file-exists-p repo-dir)
      (add-to-list 'load-path repo-dir)
      (when (file-exists-p (expand-file-name "elpaca-autoloads.el" repo-dir))
        (load (expand-file-name "elpaca-autoloads.el" repo-dir) nil t)))

     ;; Nothing available
     (t
      (when (backpack-normal-mode-p)
        (display-warning 'backpack
                         "Elpaca is not installed. Run 'backpack ensure' first."
                         :error))))))

(defun backpack--setup-elpaca-for-mode ()
  "Configure elpaca based on `backpack-mode'."
  (when (featurep 'elpaca)
    (cond
     ((backpack-sync-mode-p)
      ;; Sync mode: do everything except activation
      (setq elpaca-build-steps backpack--sync-build-steps))
     ((backpack-normal-mode-p)
      ;; Normal mode: add recipe function to prevent building unbuilt packages
      (add-to-list 'elpaca-recipe-functions #'backpack--recipe-skip-unbuilt-in-normal-mode)))))

(defun backpack--recipe-skip-unbuilt-in-normal-mode (recipe)
  "Modify RECIPE to skip building in normal mode if package isn't already built.
Returns a plist with :build set to activation-only steps for unbuilt packages."
  (when (backpack-normal-mode-p)
    (let* ((package (plist-get recipe :package))
           (build-dir (when package
                        (expand-file-name package elpaca-builds-directory)))
           (builtp (and build-dir (file-exists-p build-dir))))
      (unless builtp
        ;; Package is not built - in normal mode, we should not try to build it
        ;; Return :build nil to effectively skip this package's build
        ;; but still allow queuing (for dependency tracking)
        (message "Backpack: Package '%s' is not installed. Run 'backpack ensure'." package)
        '(:build nil)))))

;; Initialize elpaca
(backpack--ensure-elpaca)

;; Configure elpaca based on backpack mode after loading
(with-eval-after-load 'elpaca
  (backpack--setup-elpaca-for-mode)

  ;; Advise elpaca to collect package names during gc mode
  (defun backpack--elpaca-gc-advice (orig-fn order &rest body)
    "Advice for `elpaca' macro to collect package names during gc mode.
In gc mode, just collect the package name without actually queuing.
ORIG-FN is the original function, ORDER is the package order, BODY is the rest."
    (let* ((order-val (if (and (consp order) (eq (car order) 'quote))
                          (cadr order)
                        order))
           (pkg-name (cond
                      ((symbolp order-val) order-val)
                      ((consp order-val) (car order-val))
                      (t nil))))
      (if (backpack-gc-mode-p)
          ;; In gc mode, just collect the package name
          (when pkg-name
            (backpack--gc-collect-package pkg-name)
            nil)
        ;; Normal operation - call original
        (apply orig-fn order body))))

  (advice-add 'elpaca--expand-declaration :around #'backpack--elpaca-gc-advice)

  ;; In sync mode, prevent elpaca from running deferred config forms
  ;; These forms (from :config blocks) shouldn't run during package installation
  (defun backpack--elpaca-skip-forms-in-sync-mode (orig-fn q)
    "Advice to skip running deferred forms in sync mode.
ORIG-FN is `elpaca--finalize-queue', Q is the queue being finalized."
    (if (backpack-sync-mode-p)
        ;; In sync mode, clear the forms before finalization so they don't run
        (progn
          (setf (elpaca-q<-forms q) nil)
          (funcall orig-fn q))
      ;; Normal mode - run as usual
      (funcall orig-fn q)))

  (advice-add 'elpaca--finalize-queue :around #'backpack--elpaca-skip-forms-in-sync-mode))

(defvar backpack-after-init-hook nil
  "Abnormal hook for functions to be run after Backpack was initialized.")

(defun backpack--packages-need-sync-p ()
  "Return non-nil if packages need synchronization (installation/building).
This checks if the elpaca builds directory exists and has content."
  (not (and (file-exists-p elpaca-builds-directory)
            (directory-files elpaca-builds-directory nil "^[^.]" t))))

(defun backpack--check-packages-installed ()
  "Check if packages are installed, warn user if sync is needed."
  (when (and (backpack-normal-mode-p)
             (backpack--packages-need-sync-p))
    (display-warning
     'backpack
     "Packages are not installed. Run 'backpack ensure' to install them."
     :warning)))

(defun backpack--activate-packages ()
  "Activate all queued packages without attempting installation.
Used in normal mode when packages are already built.
In normal mode, elpaca will automatically use pre-built steps for
packages that already have build directories."
  (when (and (backpack-normal-mode-p) (featurep 'elpaca))
    ;; Check if packages need sync first
    (backpack--check-packages-installed)
    ;; Process queues - elpaca will automatically detect pre-built packages
    ;; and use activation-only steps for them
    (elpaca-process-queues)))

(defun backpack--sync-packages ()
  "Install and build all queued packages without activation.
Used in sync mode (`backpack ensure')."
  (when (and (backpack-sync-mode-p) (featurep 'elpaca))
    (backpack--setup-elpaca-for-mode)
    (elpaca-process-queues)))

(defun backpack-start (&optional interactive?)
  "Start the Backpack session.
When INTERACTIVE? is non-nil, we're in a normal interactive Emacs session.
The behavior depends on `backpack-mode':
- In `normal' mode: only activate pre-built packages
- In `sync' mode: install/build packages without activation"
  (when (daemonp)
    (message "Starting in daemon mode...")
    (add-hook 'kill-emacs-hook
	      (lambda ()
		(message "Killing Emacs. ¡Adiós!"))
	      100))

  ;; Ensure required directories exist
  (with-file-modes 448
    (mapc (lambda (dir)
	    (make-directory dir t))
	  (list backpack-cache-dir
		backpack-nonessential-dir
		backpack-state-dir
		backpack-data-dir
		backpack-tree-sitter-installation-dir)))

  (if interactive?
      (progn
	;; Configure appropriate hook based on mode
	(cond
	 ((backpack-normal-mode-p)
	  ;; Normal mode: activate packages after init
	  (add-hook 'backpack-after-init-hook #'backpack--activate-packages))
	 ((backpack-sync-mode-p)
	  ;; Sync mode: build packages (without activation)
	  (add-hook 'backpack-after-init-hook #'backpack--sync-packages)))

	;; last hook to run in Emacs' startup process.
	(advice-add #'command-line-1 :after #'backpack-finalize)

	;; load user's private configuration
	(let ((init-file (expand-file-name "init.el" backpack-user-dir)))
	  (load init-file t)
	  ;; load custom file
	  (load custom-file t)
	  ;; load all gears
	  (backpack-load-gear-files)))
    (progn ;; CLI/batch mode
      nil))

  ;; load site files
  (let ((site-loader
	 (lambda ()
	   (unless site-run-file
	     (when-let* ((site-file (get 'site-run-file 'initial-value)))
	       (let ((inhibit-startup-screen inhibit-startup-screen))
		 (setq site-run-file site-file)
		 (load site-run-file t)))))))

    (if interactive?
	(define-advice startup--load-user-init-file (:before (&rest _) load-site-files 100)
	  (funcall site-loader))
      (funcall site-loader)))
  t)

(defun backpack-finalize (&rest _)
  "After the startup process finalizes."
  (setq backpack-init-time (float-time (time-subtract (current-time) before-init-time)))

  ;; Run backpack hooks which will trigger package processing
  (run-hooks 'backpack-after-init-hook)

  (when (eq (default-value 'gc-cons-threshold) most-positive-fixnum)
    (setq-default gc-cons-threshold (* 16 1024 1024)))

  (when (backpack-normal-mode-p)
    (message "Backpack initialized in %.2fs" backpack-init-time))
  t)

(defun backpack-load-gear-files ()
  "Load all gears available."
  (load (expand-file-name "gears/config/default" backpack-core-dir))
  (load (expand-file-name "gears/ui/treesit" backpack-core-dir))
  (load (expand-file-name "gears/ui/theme" backpack-core-dir))
  (load (expand-file-name "gears/completion/eglot" backpack-core-dir))
  (load (expand-file-name "gears/completion/vertico" backpack-core-dir))
  (load (expand-file-name "gears/completion/orderless" backpack-core-dir))
  (load (expand-file-name "gears/completion/marginalia" backpack-core-dir))
  (load (expand-file-name "gears/completion/nerd-icons-completion" backpack-core-dir))
  (load (expand-file-name "gears/completion/corfu" backpack-core-dir))
  (load (expand-file-name "gears/tools/magit" backpack-core-dir))
  (load (expand-file-name "gears/tools/whitespaces" backpack-core-dir))
  (load (expand-file-name "gears/tools/envrc" backpack-core-dir))
  (load (expand-file-name "gears/tools/eldoc" backpack-core-dir))
  (load (expand-file-name "gears/checkers/spellchecking" backpack-core-dir))
  (load (expand-file-name "gears/email/mu4e" backpack-core-dir))
  (load (expand-file-name "gears/editing/emacs-lisp" backpack-core-dir))
  (load (expand-file-name "gears/editing/go" backpack-core-dir))
  (load (expand-file-name "gears/editing/org" backpack-core-dir))
  (load (expand-file-name "gears/editing/hyprland" backpack-core-dir))
  (load (expand-file-name "gears/editing/nix" backpack-core-dir))
  (load (expand-file-name "gears/editing/python" backpack-core-dir))
  (load (expand-file-name "gears/editing/lua" backpack-core-dir))
  (load (expand-file-name "gears/editing/json" backpack-core-dir)))

;;; Garbage Collection (orphaned packages cleanup)

(defvar backpack--gc-mode nil
  "When non-nil, we're in garbage collection mode.
In this mode, we collect package names without actually installing them.")

(defvar backpack--queued-packages nil
  "List of package names that would be queued based on current configuration.
This is populated during gc mode.")

(defun backpack-gc-mode-p ()
  "Return non-nil if Backpack is in garbage collection mode."
  (eq backpack--gc-mode t))

(defun backpack--gc-collect-package (package-name)
  "Add PACKAGE-NAME to the list of queued packages during gc collection."
  (when (and package-name (symbolp package-name))
    (cl-pushnew package-name backpack--queued-packages)))

(defun backpack--get-installed-packages ()
  "Return a list of package names that are currently installed (have build dirs)."
  (when (file-exists-p elpaca-builds-directory)
    (mapcar #'intern
            (cl-remove-if
             (lambda (name) (member name '("." "..")))
             (directory-files elpaca-builds-directory nil "^[^.]")))))

(defun backpack--get-repo-packages ()
  "Return a list of package names that have repos cloned."
  (when (file-exists-p elpaca-repos-directory)
    (mapcar #'intern
            (cl-remove-if
             (lambda (name) (member name '("." "..")))
             (directory-files elpaca-repos-directory nil "^[^.]")))))

(defun backpack--get-package-dependencies (package-name)
  "Get the dependencies of PACKAGE-NAME by reading its main elisp file.
Returns a list of dependency package names (symbols)."
  (let* ((build-dir (expand-file-name (symbol-name package-name) elpaca-builds-directory))
         (repo-dir (expand-file-name (symbol-name package-name) elpaca-repos-directory))
         (pkg-name-str (symbol-name package-name))
         ;; Try to find the main file or -pkg.el file
         (main-file (or (let ((f (expand-file-name (concat pkg-name-str ".el") build-dir)))
                          (and (file-exists-p f) f))
                        (let ((f (expand-file-name (concat pkg-name-str ".el") repo-dir)))
                          (and (file-exists-p f) f))
                        (let ((f (expand-file-name (concat pkg-name-str "-pkg.el") build-dir)))
                          (and (file-exists-p f) f))
                        (let ((f (expand-file-name (concat pkg-name-str "-pkg.el") repo-dir)))
                          (and (file-exists-p f) f)))))
    (when main-file
      (with-temp-buffer
        (insert-file-contents main-file)
        (goto-char (point-min))
        (condition-case nil
            (if (string-suffix-p "-pkg.el" main-file)
                ;; Parse -pkg.el format: (define-package ... DEPS ...)
                (let ((form (read (current-buffer))))
                  (when (eq (car form) 'define-package)
                    (mapcar #'car (nth 4 form))))
              ;; Parse Package-Requires header
              (when (re-search-forward "^;+[ \t]*Package-Requires[ \t]*:[ \t]*" nil t)
                (let ((deps-str (buffer-substring-no-properties (point) (line-end-position))))
                  ;; Handle multi-line Package-Requires
                  (forward-line 1)
                  (while (looking-at "^;+[ \t]+\\([^;].*\\)")
                    (setq deps-str (concat deps-str " " (match-string 1)))
                    (forward-line 1))
                  (condition-case nil
                      (mapcar #'car (read deps-str))
                    (error nil)))))
          (error nil))))))

(defun backpack--collect-all-dependencies (packages)
  "Collect all transitive dependencies for PACKAGES.
Returns a list of all packages including dependencies."
  (let ((all-packages (copy-sequence packages))
        (to-process (copy-sequence packages))
        (processed nil))
    (while to-process
      (let* ((pkg (pop to-process))
             (deps (backpack--get-package-dependencies pkg)))
        (push pkg processed)
        (dolist (dep deps)
          (unless (or (eq dep 'emacs)  ; Skip emacs itself
                      (memq dep all-packages)
                      (memq dep processed))
            (push dep all-packages)
            (push dep to-process)))))
    all-packages))

(defun backpack--find-orphaned-packages ()
  "Find packages that are installed but not needed by current configuration.
Returns a list of orphaned package names."
  (let* ((installed (backpack--get-installed-packages))
         ;; Expand queued packages to include all their dependencies
         (needed-with-deps (backpack--collect-all-dependencies backpack--queued-packages)))
    (cl-set-difference installed needed-with-deps)))

(defun backpack--delete-package (package-name)
  "Delete PACKAGE-NAME's build and repo directories."
  (let ((build-dir (expand-file-name (symbol-name package-name) elpaca-builds-directory))
        (repo-dir (expand-file-name (symbol-name package-name) elpaca-repos-directory)))
    (when (file-exists-p build-dir)
      (message "Backpack GC: Deleting build directory for %s..." package-name)
      (delete-directory build-dir t))
    (when (file-exists-p repo-dir)
      (message "Backpack GC: Deleting repo directory for %s..." package-name)
      (delete-directory repo-dir t))))

(defun backpack--gc-delete-orphaned-packages (orphaned-packages)
  "Delete all ORPHANED-PACKAGES from disk."
  (dolist (pkg orphaned-packages)
    (backpack--delete-package pkg)))

(defun backpack--calculate-directory-size (directory)
  "Calculate the total size of DIRECTORY in bytes."
  (let ((total 0))
    (when (file-exists-p directory)
      (dolist (file (directory-files-recursively directory ".*" t))
        (unless (file-directory-p file)
          (setq total (+ total (or (file-attribute-size (file-attributes file)) 0))))))
    total))

(defun backpack--format-size (bytes)
  "Format BYTES as a human-readable string."
  (cond
   ((>= bytes (* 1024 1024 1024))
    (format "%.2f GB" (/ bytes (* 1024.0 1024.0 1024.0))))
   ((>= bytes (* 1024 1024))
    (format "%.2f MB" (/ bytes (* 1024.0 1024.0))))
   ((>= bytes 1024)
    (format "%.2f KB" (/ bytes 1024.0)))
   (t (format "%d bytes" bytes))))

(defun backpack-gc (&optional dry-run)
  "Remove orphaned packages that are no longer needed.
If DRY-RUN is non-nil, only report what would be deleted without deleting."
  (setq backpack--queued-packages nil)
  (setq backpack--gc-mode t)

  ;; Load user configuration to get gear declarations
  (let ((init-file (expand-file-name "init.el" backpack-user-dir)))
    (when (file-exists-p init-file)
      (load init-file t)))

  ;; Load all gears to collect package names
  ;; The elpaca/leaf macros will call backpack--gc-collect-package in gc mode
  (backpack-load-gear-files)

  (setq backpack--gc-mode nil)

  ;; Always keep elpaca itself
  (cl-pushnew 'elpaca backpack--queued-packages)

  (let ((orphaned (backpack--find-orphaned-packages)))
    (if (null orphaned)
        (message "Backpack GC: No orphaned packages found. Nothing to clean up.")
      (let ((total-size 0))
        ;; Calculate size of orphaned packages
        (dolist (pkg orphaned)
          (let ((build-dir (expand-file-name (symbol-name pkg) elpaca-builds-directory))
                (repo-dir (expand-file-name (symbol-name pkg) elpaca-repos-directory)))
            (setq total-size (+ total-size
                                (backpack--calculate-directory-size build-dir)
                                (backpack--calculate-directory-size repo-dir)))))

        (message "")
        (message "Backpack GC: Found %d orphaned package(s):" (length orphaned))
        (dolist (pkg orphaned)
          (message "  - %s" pkg))
        (message "")
        (message "Total space to be freed: %s" (backpack--format-size total-size))
        (message "")

        (if dry-run
            (message "Backpack GC: Dry run - no packages were deleted.")
          (backpack--gc-delete-orphaned-packages orphaned)
          (message "")
          (message "Backpack GC: Deleted %d orphaned package(s), freed %s."
                   (length orphaned)
                   (backpack--format-size total-size)))))))

(provide 'backpack)
