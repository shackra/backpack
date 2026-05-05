;; -*- lexical-binding: t; -*-

;; -- Toggle & project helpers --

(defun backpack/eshell-toggle ()
  "Toggle an eshell buffer in a bottom side window.
If an eshell window is visible, delete it.  Otherwise open one."
  (interactive)
  (if-let* ((buf (get-buffer "*eshell*"))
            (win (get-buffer-window buf)))
      (delete-window win)
    (if (get-buffer "*eshell*")
        (pop-to-buffer "*eshell*")
      (eshell))))

(defun backpack/eat-toggle ()
  "Toggle an eat terminal buffer in a bottom side window.
If an eat window is visible, delete it.  Otherwise open one."
  (interactive)
  (if-let* ((buf (get-buffer "*eat*"))
            (win (get-buffer-window buf)))
      (delete-window win)
    (if (get-buffer "*eat*")
        (pop-to-buffer "*eat*")
      (eat))))

(defun backpack/eshell-project ()
  "Open eshell at the current project root.
Creates a per-project buffer named *eshell:<project>*."
  (interactive)
  (let* ((pr (project-current t))
         (root (project-root pr))
         (name (format "*eshell:%s*" (file-name-nondirectory
                                      (directory-file-name root))))
         (default-directory root))
    (if-let* ((buf (get-buffer name))
              (win (get-buffer-window buf)))
        (delete-window win)
      (if (get-buffer name)
          (pop-to-buffer name)
        (let ((eshell-buffer-name name))
          (eshell t))))))

(defun backpack/eat-project ()
  "Open eat at the current project root.
Creates a per-project buffer named *eat:<project>*."
  (interactive)
  (let* ((pr (project-current t))
         (root (project-root pr))
         (name (format "*eat:%s*" (file-name-nondirectory
                                     (directory-file-name root))))
         (default-directory root))
    (if-let* ((buf (get-buffer name))
              (win (get-buffer-window buf)))
        (delete-window win)
      (if (get-buffer name)
          (pop-to-buffer name)
        (let ((eat-buffer-name name))
          (eat))))))

;; -- Dispatch function for C-c t p --
;; This file loads after vterm.el, so this definition takes precedence.
;; Priority: vterm > eat > eshell.

(defun backpack/term-project ()
  "Open a terminal at the current project root.
Prefers vterm when active, then eat, then eshell."
  (interactive)
  (cond
   ((and (gearp! :term vterm)
         (fboundp 'backpack/vterm-project))
    (backpack/vterm-project))
   ((and (gearp! :term eshell)
         (not (gearp! :term eshell -eat))
         (fboundp 'backpack/eat-project))
    (backpack/eat-project))
   ((fboundp 'backpack/eshell-project)
    (backpack/eshell-project))
   (t (error "No terminal gear enabled"))))

;; -- Display rule: eshell buffers always appear at the bottom --

(when (gearp! :term eshell)
  (add-to-list 'display-buffer-alist
               '("\\*eshell"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.35))))

;; -- Main leaf block: eshell (builtin) --

(leaf eshell
  :doc "The Emacs shell -- a shell written entirely in Emacs Lisp"
  :when (gearp! :term eshell)
  :tag "builtin"
  :bind
  ("C-c t e" . backpack/eshell-toggle)
  ("C-c t p" . backpack/term-project)
  :custom
  ;; Truncate buffer to this many lines (0 = no truncation)
  (eshell-buffer-maximum-lines . 10000)
  ;; Scrollback behaviour: scroll to bottom on output
  (eshell-scroll-to-bottom-on-output . t)
  ;; Destroy finished processes immediately
  (eshell-destroy-buffer-when-process-dies . t)
  :config
  ;; Ensure UTF-8 for sub-processes launched from eshell
  (add-hook 'eshell-mode-hook
            (lambda ()
              (set-buffer-file-coding-system 'utf-8-unix)
              (add-hook 'kill-buffer-hook
                        (lambda ()
                          (when-let* ((win (get-buffer-window)))
                            (delete-window win)))
                        nil t))))

;; -- eat integration (default-on flag; opt-out with -eat) --

(when (and (gearp! :term eshell)
           (not (gearp! :term eshell -eat)))
  ;; Display rule for standalone eat buffers
  (add-to-list 'display-buffer-alist
               '("\\*eat"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.35))))

(leaf eat
  :doc "Emulate A Terminal -- fast terminal emulator for Emacs"
  :when (and (gearp! :term eshell)
             (not (gearp! :term eshell -eat)))
  :ensure (eat :host codeberg :repo "akib/emacs-eat"
               :ref "c8d54d649872bfe7b2b9f49ae5c2addbf12d3b99")
  :preface
  ;; Eat hardcodes "/usr/bin/env" "sh" for a stty+exec bootstrap; that path
  ;; does not exist on native Windows.  Git Bash / MSYS2 provide bash.exe.
  (defun backpack--make-process-eat-windows (orig &rest plist)
    "Around-advice for `make-process'.
Rewrite eat's POSIX-only bootstrap command when bash is available."
    (let ((cmd (plist-get plist :command))
          (bash (backpack--windows-posix-bash-executable)))
      (if (and backpack--system-windows-p
               bash
               (listp cmd)
               (>= (length cmd) 4)
               (equal (car cmd) "/usr/bin/env")
               (equal (nth 1 cmd) "sh")
               (equal (nth 2 cmd) "-c")
               (stringp (nth 3 cmd))
               (string-match-p "\\bstty\\b" (nth 3 cmd)))
          (let* ((tail (nthcdr 3 cmd))
                 (script (car tail))
                 ;; Eat sends stderr to `null-device' (NUL); MSYS bash treats
                 ;; that poorly — use /dev/null like on Unix.
                 (script-fixed (if (stringp script)
                                   (replace-regexp-in-string
                                    "2>[Nn][Uu][Ll]" "2>/dev/null" script)
                                 script))
                 (tail-fixed (cons script-fixed (cdr tail))))
            (apply orig (plist-put (copy-tree plist) :command
                                   (cons bash (cons "-c" tail-fixed)))))
        (apply orig plist))))
  :bind
  ("C-c t a" . backpack/eat-toggle)
  :custom
  ;; Large scrollback for the eat terminal (in bytes)
  (eat-term-scrollback-size . 131072)
  ;; Enable automatic line rewrapping on window resize
  (eat-enable-auto-line-translation . t)
  :config
  (when-let ((bash (backpack--windows-posix-bash-executable)))
    (setq eat-shell bash)
    (advice-remove #'make-process #'backpack--make-process-eat-windows)
    (advice-add #'make-process :around #'backpack--make-process-eat-windows))
  ;; Integrate eat with eshell -- this makes eshell handle TUI/curses
  ;; applications correctly by delegating rendering to eat.
  (eat-eshell-mode +1)
  ;; Also let eat handle "visual" commands (less, htop, etc.)
  (eat-eshell-visual-command-mode +1)
  (add-hook 'eat-exit-hook
            (lambda (_process)
              (when-let* ((win (get-buffer-window)))
                (delete-window win)))))
