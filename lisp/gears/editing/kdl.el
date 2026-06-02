;; -*- lexical-binding: t -*-
;; Declare tree-sitter languages needed by this gear
(when (backpack-treesit-was-asked 'kdl)
  (backpack-treesit-recipe! kdl
    :url "https://github.com/tree-sitter-grammars/tree-sitter-kdl"
    :ext "\\.kdl\\'"
    :ts-mode 'kdl-ts-mode
    :versions ((:until-abi 14 :revision "b37e3d58e5c5cf8d739b315d6114e02d42e66664"))))

(leaf kdl-mode
  :doc "the KDL document language — nodes, values, and types, no curly-brace soup"
  :ensure (kdl-mode :host github :repo "taquangtrung/emacs-kdl-mode" :ref "2d849e298199f490e4894c01764a8a83decd704a")
  :when (gearp! :editing kdl)
  :hook
  ((kdl-mode-hook kdl-ts-mode-hook) . electric-pair-local-mode)
  ((kdl-mode-hook kdl-ts-mode-hook) .
   (lambda ()
     (toggle-truncate-lines +1)
     (unless (gearp! :editing kdl -display-line-numbers)
       (display-line-numbers-mode +1)))))

(leaf kdl-ts-mode
  :doc "tree-sitter major mode for KDL files"
  :ensure (kdl-ts-mode :host github :repo "merrickluo/kdl-ts-mode" :ref "90cf3a9934c066d8613dee94816aa59be3ed1d00")
  :unless (gearp! :editing kdl -treesit)
  :after kdl-mode)
