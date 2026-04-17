(leaf eldoc
  :doc "Originally Emacs Lisp Documentation, now it offers documentation lookup for all major modes"
  :unless (gearp! :tools -eldoc)
  :global-minor-mode global-eldoc-mode)

(leaf eldoc-box
  :doc "Display eldoc documentation in a childframe at point. GUI only.
In terminal, use C-h . to show documentation in a side window."
  :when (and (gearp! :tools eldoc box) (display-graphic-p))
  :ensure (eldoc-box :ref "fead2cef661790417267e5498d4d14806e020f99")
  :custom
  (eldoc-box-hover-display-frame-above-point . t)
  :hook
  (eldoc-mode-hook . eldoc-box-hover-at-point-mode))

(when (and (gearp! :tools eldoc box) (not (display-graphic-p)))
  (setq eldoc-echo-area-use-multiline-p nil)
  (setq eldoc-echo-area-prefer-doc-buffer 'maybe)
  (setq eldoc-echo-area-display-truncation-message t)
  (add-to-list 'display-buffer-alist
               '("\\*eldoc\\*"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.15)
                 (slot . 0)
                 (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t))))))
