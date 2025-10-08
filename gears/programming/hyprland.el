(require 'backpack-pouch)

(leaf hyprlang-ts-mode
  :when (gearp! :programming hyprland)
  :mode "/hypr/.*\\.conf\\'"
  :custom (hyprlang-ts-mode-indent-offset . 2)
  :config
  (leaf eglot
    :doc "Language Server Protocol support"
    :when (gearp! :programming hyprland lsp)
    :doctor ("hyprls" . "LSP server for Hyprland configuration files")
    :hook (hyprlang-ts-mode-hook . eglot-ensure)
    :config
    (add-to-list 'eglot-server-programs '(hyprlang-ts-mode . ("hyprls")))))
