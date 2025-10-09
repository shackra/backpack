(require 'backpack-pouch)

(leaf marginalia
  :doc "adds marginalia to the minibuffer completions"
  :unless (gearp! :completion -marginalia)
  :ensure (marginalia :ref "18d2d02cdb6689d7d08cd6c41b497117eeeba728")
  :bind (:minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :leaf-defer nil
  :config
  (marginalia-mode))
