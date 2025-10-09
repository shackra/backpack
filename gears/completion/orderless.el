(require 'backpack-pouch)

(leaf orderless
  :unless (gearp! :completion -orderless)
  :ensure (orderless :ref "9cf1c90e2501566ceba59f3220b4630995004efd")
  :custom
  (completion-styles . '(orderless basic))
  (completion-category-overrides . '((file (styles partial-completion))))
  (completion-category-defaults . nil)) ;; Disable defaults, use orderless settings

(leaf orderless
  :unless (gearp! :completion -orderless)
  :emacs>= '31
  :custom
  (completion-pcm-leading-wildcard . t)) ;; partial-completion behaves like substring
