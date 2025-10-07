(require 'backpack-pouch)

(leaf jinx
  :doc "a fast just-in-time spell-checker"
  :ensure (jinx :ref "55c844066601ec1a4741ff8f459ff6321f6935b7")
  :when (gearp! :checkers spellchecking)
  :hook ((text-mode-hook prog-mode-hook) . jinx-mode)
  :bind ("C-;" . jinx-correct)
  :custom
  (jinx-camel-modes . '(prog-mode))
  (jinx-delay . 0.01))
