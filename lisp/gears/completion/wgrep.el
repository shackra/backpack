(leaf wgrep
  :doc "edit grep buffers and apply changes to files.
Used with embark-export to enable multi-file refactoring."
  :when (gearp! :completion embark)
  :ensure (wgrep :ref "49f09ab9b706d2312cab1199e1eeb1bcd3f27f6f")
  :custom
  (wgrep-auto-save-buffer . t)
  :bind
  (:wgrep-mode-map
   ("<C-return>" . wgrep-finish-edit)))
