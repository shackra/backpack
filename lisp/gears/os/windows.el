;; -*- lexical-binding: t; -*-
(leaf windows
  :when (gearp! :os windows)
  :doc "Performance optimizations for native Windows.

For maximum throughput, also add an exclusion in Windows Defender for
the Emacs install directory (e.g. C:/tools/emacs/) and your Backpack
cache (~/.emacs.d/.cache/).  Defender's real-time scan otherwise re-
scans every .el/.elc/.eln file Emacs touches, and that single change
typically dominates startup and `backpack ensure' time more than every
Lisp-level setting in this gear combined."
  :leaf-defer nil
  :config

  ;; ─── Process / subprocess ───
  ;; Windows CreateProcess + pipe IPC is slow.
  ;; w32-pipe-read-delay default > 0 is for ancient DOS programs.
  (when (boundp 'w32-pipe-read-delay)
    (setq w32-pipe-read-delay 0))
  ;; Larger pipe + read buffers cut syscalls per LSP message.  Servers
  ;; like gopls, rust-analyzer, and clangd routinely emit > 256 KB
  ;; JSON-RPC frames during workspace indexing.  256 KB pairs well
  ;; with a 1 MiB read buffer without inflating per-subprocess RSS.
  (when (boundp 'w32-pipe-buffer-size)
    (setq w32-pipe-buffer-size (* 256 1024)))
  (when (boundp 'w32-quote-process-args)
    (setq w32-quote-process-args t))
  ;; Use pipes instead of ptys — Windows has no real ptys,
  ;; conpty adds overhead. Safe: eshell is Elisp, vterm manages
  ;; its own pty internally.
  (setq-default process-connection-type nil)
  ;; LSP-mode and eglot both recommend >= 1 MiB on Windows.
  (setq read-process-output-max (* 1024 1024))

  ;; ─── File I/O ───
  ;; NTFS stat is 5-10× slower; skip owner/mode lookups for local
  ;; files.  The 'local value preserves accurate attributes for
  ;; remote/Tramp paths (e.g. \\\\server\\share, /ssh:host:/path)
  ;; where the cost is dominated by the network round-trip anyway.
  (when (boundp 'w32-get-true-file-attributes)
    (setq w32-get-true-file-attributes 'local))
  (when (boundp 'w32-get-true-file-link-count)
    (setq w32-get-true-file-link-count nil))

  ;; ─── Display / rendering ───
  ;; Windows font rendering (Uniscribe/DirectWrite) is expensive;
  ;; don't let GC compact font caches and force re-rasterization
  (setq inhibit-compacting-font-caches t)
  (setq fast-but-imprecise-scrolling t)
  (when (boundp 'redisplay-skip-fontification-on-input)
    (setq redisplay-skip-fontification-on-input t))
  (setq cursor-in-non-selected-windows nil
        highlight-nonselected-windows nil)
  ;; For LTR-only users: skip bidi algorithm entirely
  ;; Opt out with: (gear! :os (windows bidi))
  (unless (gearp! :os windows bidi)
    (setq bidi-display-reordering 'left-to-right
          bidi-paragraph-direction 'left-to-right
          bidi-inhibit-bpa t))
  ;; Case-insensitive FS: no point re-scanning auto-mode-alist
  (setq auto-mode-case-fold nil)

  ;; ─── Native compilation ───
  ;; Windows I/O is slower; defer async compilation to reduce contention
  (when (boundp 'native-comp-defer-time)
    (setq native-comp-defer-time 1.0))

  ;; ─── Network ───
  ;; Reuse TLS connections — avoid repeated CryptoAPI handshake overhead
  (setq url-http-attempt-keepalives t)

  ;; ─── Shell auto-detection ───
  ;; Prefer bash (Git for Windows / MSYS2) if available;
  ;; fall back to cmdproxy.exe (bundled with Emacs)
  (when (and (or (null shell-file-name)
                 (string-match-p "cmd\\(proxy\\)?\\.exe" shell-file-name))
             (executable-find "bash"))
    (setq shell-file-name (executable-find "bash")
          shell-command-switch "-c")))

(leaf windows-legacy
  :when (gearp! :os windows)
  :emacs< "30.1"
  :doc "Defensive Windows perf settings for Emacs 29.x.

These variables are already auto-detected or correctly defaulted in
Emacs 30.1, so setting them there is redundant.  On 29.1-29.4 the
defaults are the same in stock Emacs, but vendor builds and downstream
patches occasionally diverge — pinning the values here guarantees the
performance characteristics regardless of how the binary was built.

All sets are guarded with `boundp' so an unexpectedly-stripped build
cannot signal a void-variable error during gear initialization."
  :leaf-defer nil
  :config

  ;; ─── Display / rendering ───
  ;; Use the native GDI+ image decoder instead of the bundled libpng /
  ;; libjpeg / libtiff path: faster, fewer DLL dependencies, and no
  ;; CVE risk from stale image libraries.  Auto-detected as t in 30.1
  ;; whenever GDI+ loads; pin it for 29.x where some builds leave it nil.
  (when (boundp 'w32-use-native-image-API)
    (setq w32-use-native-image-API t))
  ;; Keep per-frame double buffering on globally — disabling it causes
  ;; visible tearing on modern Windows compositors.  Default is nil in
  ;; both 29.x and 30.x; explicit for safety.
  (when (boundp 'w32-disable-double-buffering)
    (setq w32-disable-double-buffering nil))
  ;; Keep the modern Uniscribe shaping path (added in Emacs 25); the
  ;; legacy fallback is slower and renders some scripts incorrectly.
  (when (boundp 'w32-disable-new-uniscribe-apis)
    (setq w32-disable-new-uniscribe-apis nil))

  ;; ─── Process / subprocess ───
  ;; Adaptive read buffering throttles output from chatty subprocesses
  ;; (compilers, LSP servers).  On Windows the heuristic interacts
  ;; badly with pipe IPC and starves large bursts; force it off so
  ;; `read-process-output-max' / `w32-pipe-buffer-size' can do their job.
  (when (boundp 'process-adaptive-read-buffering)
    (setq process-adaptive-read-buffering nil)))