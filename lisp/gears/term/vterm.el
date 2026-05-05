;; -*- lexical-binding: t; -*-

;; -- Toggle & project helpers --

(defun backpack/vterm-toggle ()
  "Toggle a vterm buffer in a bottom side window.
If a vterm window is visible, delete it.  Otherwise open one."
  (interactive)
  (if-let* ((buf (get-buffer "*vterm*"))
            (win (get-buffer-window buf)))
      (delete-window win)
    (if (get-buffer "*vterm*")
        (pop-to-buffer "*vterm*")
      (vterm "*vterm*"))))

(defun backpack/vterm-project ()
  "Open vterm at the current project root.
Creates a per-project buffer named *vterm:<project>*."
  (interactive)
  (let* ((pr (project-current t))
         (root (project-root pr))
         (name (format "*vterm:%s*" (file-name-nondirectory
                                     (directory-file-name root))))
         (default-directory root))
    (if-let* ((buf (get-buffer name))
              (win (get-buffer-window buf)))
        (delete-window win)
      (if (get-buffer name)
          (pop-to-buffer name)
        (vterm name)))))

;; -- Display rule: vterm buffers always appear at the bottom --

(when (gearp! :term vterm)
  (add-to-list 'display-buffer-alist
               '("\\*vterm"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.35))))

;; -- Main leaf block --

(leaf vterm
  :doc "Fully-featured terminal emulator based on libvterm"
  :when (gearp! :term vterm)
  :ensure (vterm :ref "54c29d14bca05bdd8ae60cda01715d727831e3f9")
  :doctor ("cmake"   . ("needed to compile vterm's C module" required))
  :doctor ("libtool" . ("needed to compile vterm's C module" required))
  :bind
  ("C-c t v" . backpack/vterm-toggle)
  ("C-c t p" . backpack/term-project)
  :custom
  ;; Large scrollback (number of lines kept in memory)
  (vterm-max-scrollback . 10000)
  ;; Always use UTF-8
  (vterm-environment . '("LANG=en_US.UTF-8"))
  ;; Kill the buffer when the shell exits
  (vterm-kill-buffer-on-exit . t)
  :config
  ;; Directory tracking -- vterm reads OSC 7 escape sequences emitted by
  ;; the shell to keep `default-directory' in sync.  Add the following
  ;; snippet to your shell RC file (bash/zsh):
  ;;
  ;;   vterm_printf() { printf "\e]%s\e\\" "$1"; }
  ;;   vterm_prompt_end() { vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"; }
  ;;   PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND; }vterm_prompt_end"
  ;;
  ;; For fish:
  ;;   function vterm_prompt_end; printf '\e]51;A%s@%s:%s\e\\' $USER $hostname $PWD; end
  ;;   functions -c fish_prompt _old_fish_prompt
  ;;   function fish_prompt; _old_fish_prompt; vterm_prompt_end; end
  ;;
  ;; zsh users can alternatively source the bundled helpers:
  ;;   source <path-to-emacs-backpack>/.cache/nonessentials/elpaca/builds/vterm/etc/emacs-vterm-zsh.sh
  (add-hook 'vterm-mode-hook
            (lambda ()
              ;; Ensure the process uses UTF-8
              (set-process-coding-system (get-buffer-process (current-buffer))
                                        'utf-8-unix 'utf-8-unix)))
  (add-hook 'vterm-exit-functions
            (lambda (&rest _)
              (when-let* ((win (get-buffer-window)))
                (delete-window win)))))
