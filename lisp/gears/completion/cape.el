(leaf cape
  :doc "completion-at-point extensions: dabbrev, file, keyword, etc.
Enriches Corfu with additional completion backends."
  :when (gearp! :completion cape)
  :ensure (cape :ref "74d37ab3e1b8d11c3871503c46720b3ed8d4c503")
  :leaf-defer nil
  :bind
  ("M-p d" . cape-dabbrev)
  ("M-p f" . cape-file)
  ("M-p k" . cape-keyword)
  ("M-p s" . cape-elisp-symbol)
  ("M-p l" . cape-line)
  ("M-p w" . cape-dict)
  ("M-p h" . cape-history)
  ("M-p :" . cape-emoji)
  :init
  (add-hook 'completion-at-point-functions #'cape-file 20)
  (add-hook 'completion-at-point-functions #'cape-dabbrev 30))
