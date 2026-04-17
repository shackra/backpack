(leaf eldoc
  :doc "Originally Emacs Lisp Documentation, now it offers documentation lookup for all major modes"
  :unless (gearp! :tools -eldoc)
  :global-minor-mode global-eldoc-mode)

(leaf eldoc-box
  :doc "Display eldoc documentation in a childframe at point (GUI) or concise echo area (TTY).
Use C-h . to show documentation in a side window; C-u C-h . to dismiss it."
  :when (gearp! :tools eldoc box)
  :ensure (eldoc-box :ref "fead2cef661790417267e5498d4d14806e020f99")
  :custom
  (eldoc-box-hover-display-frame-above-point . t)
  :hook
  (eldoc-mode-hook . (lambda ()
                       (if (display-graphic-p)
                           (eldoc-box-hover-at-point-mode +1)
                         (setq-local eldoc-echo-area-use-multiline-p nil)
                         (setq-local eldoc-echo-area-prefer-doc-buffer 'maybe)
                         (setq-local eldoc-echo-area-display-truncation-message t)))))

(when (gearp! :tools eldoc box)
  (defun backpack--display-eldoc-side-window (buffer alist)
    (display-buffer-in-side-window
     buffer
     `((side . right)
       (window-width . 0.25)
       (slot . 0)
       (window-parameters . ((no-other-window . t)
                             (no-delete-other-windows . t))))))

  (add-to-list 'display-buffer-alist
               '("\\*eldoc\\*" backpack--display-eldoc-side-window))

  (defvar-local backpack--eldoc-box-was-on nil
    "Whether eldoc-box-hover-at-point-mode was active before C-h . toggled it off.")

  (define-advice eldoc-doc-buffer (:around (fn &rest args) toggle-box)
    (if current-prefix-arg
        (progn
          (let ((win (get-buffer-window "*eldoc*")))
            (when win (quit-window nil win)))
          (when (and (display-graphic-p) backpack--eldoc-box-was-on)
            (eldoc-box-hover-at-point-mode +1)
            (setq-local backpack--eldoc-box-was-on nil)))
      (when (and (display-graphic-p) (bound-and-true-p eldoc-box-hover-at-point-mode))
        (setq-local backpack--eldoc-box-was-on t)
        (eldoc-box-hover-at-point-mode -1))
      (apply fn args))))
