;; -*- lexical-binding: t; -*-

;; -- Toggle & project helpers --

(when (gearp! :term powershell)
  (defun backpack/powershell-toggle ()
    "Toggle a PowerShell inferior buffer in a bottom side window.
If a PowerShell window is visible, delete it.  Otherwise open one."
    (interactive)
    (if-let* ((buf (get-buffer "*PowerShell*"))
              (win (get-buffer-window buf)))
        (delete-window win)
      (if (get-buffer "*PowerShell*")
          (pop-to-buffer "*PowerShell*")
        (powershell "*PowerShell*"))))

  (defun backpack/powershell-project ()
    "Open PowerShell at the current project root.
Creates a per-project buffer named *PowerShell:<project>*."
    (interactive)
    (let* ((pr (project-current t))
           (root (project-root pr))
           (name (format "*PowerShell:%s*" (file-name-nondirectory
                                            (directory-file-name root))))
           (default-directory root))
      (if-let* ((buf (get-buffer name))
                (win (get-buffer-window buf)))
          (delete-window win)
        (if (get-buffer name)
            (pop-to-buffer name)
          (powershell name))))))

;; -- Display rule: PowerShell buffers appear at the bottom --

(when (gearp! :term powershell)
  (add-to-list 'display-buffer-alist
               '("\\*PowerShell"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.35))))

;; -- Main leaf block --

(leaf powershell
  :doc "Inferior PowerShell shell inside Emacs"
  :when (gearp! :term powershell)
  :ensure (powershell :ref "ae60e11c96cc1767f05ce0cab6a917240ce2e37a")
  :doctor ("pwsh" . ("PowerShell 7+ cross-platform shell" required))
  :bind
  ("C-c t s" . backpack/powershell-toggle)
  ("C-c t p" . backpack/term-project)
  :config
  ;; Kill buffer and close window when the PowerShell process exits or is killed.
  (add-hook 'comint-exec-hook
            (lambda ()
              (when (string-prefix-p "*PowerShell" (buffer-name))
                (add-hook 'kill-buffer-hook
                          (lambda ()
                            (when-let* ((win (get-buffer-window)))
                              (delete-window win)))
                          nil t)
                (when-let* ((proc (get-buffer-process (current-buffer))))
                  (set-process-sentinel
                   proc
                   (lambda (p _event)
                     (unless (process-live-p p)
                       (when-let* ((buf (process-buffer p)))
                         (when (buffer-live-p buf)
                           (let ((win (get-buffer-window buf t)))
                             (kill-buffer buf)
                             (when (and win (window-live-p win))
                               (ignore-errors (delete-window win))))))))))))))



