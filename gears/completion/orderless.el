(require 'backpack-pouch)

(leaf orderless
  :doc "an orderless «completion style» that divides the pattern into space-separated components, and matches candidates that match all of the components in any order"
  :unless (gearp! :completion -orderless)
  :ensure (orderless :ref "9cf1c90e2501566ceba59f3220b4630995004efd")
  :custom
  (completion-styles . '(orderless basic))
  (completion-category-overrides . '((file (styles partial-completion orderless))))
  (completion-category-defaults . nil) ;; Disable defaults, use orderless settings
  (orderless-matching-styles . '(orderless-literal
				 orderless-prefixes
				 orderless-initialism
				 orderless-regexp)))

(leaf orderless
  :unless (gearp! :completion -orderless)
  :emacs>= '31
  :custom
  (completion-pcm-leading-wildcard . t)) ;; partial-completion behaves like substring
