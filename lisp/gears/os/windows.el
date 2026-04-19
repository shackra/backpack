;; -*- lexical-binding: t; -*-
(leaf windows
  :when (gearp! :os windows)
  :doc "Performance optimizations for native Windows"
  :leaf-defer nil
  :config

  ;; ─── Process / subprocess ───
  ;; Windows CreateProcess + pipe IPC is slow.
  ;; w32-pipe-read-delay default > 0 is for ancient DOS programs.
  (when (boundp 'w32-pipe-read-delay)
    (setq w32-pipe-read-delay 0))
  (when (boundp 'w32-pipe-buffer-size)
    (setq w32-pipe-buffer-size (* 64 1024)))
  (when (boundp 'w32-quote-process-args)
    (setq w32-quote-process-args t))
  ;; Use pipes instead of ptys — Windows has no real ptys,
  ;; conpty adds overhead. Safe: eshell is Elisp, vterm manages
  ;; its own pty internally.
  (setq-default process-connection-type nil)
  ;; Larger read chunks amortize syscall overhead
  (setq read-process-output-max (* 64 1024))

  ;; ─── File I/O ───
  ;; NTFS stat is 5-10× slower; skip owner/mode lookups
  (when (boundp 'w32-get-true-file-attributes)
    (setq w32-get-true-file-attributes nil))
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