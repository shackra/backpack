;; Declare tree-sitter languages needed by this gear
(when (and (gearp! :editing dart)
           (not (gearp! :editing dart -treesit)))
  (backpack-treesit-recipe! dart
    :ts-mode 'dart-ts-mode
    :remap 'dart-mode
    :url "https://github.com/UserNobody14/tree-sitter-dart"
    :ext "\\.dart\\'"
    :versions ((:until-abi 14 :revision "c1222f5a65aba7e0175cc0cc6f88d198d9fe2b02"))))

(leaf dart-mode
  :doc "the language where everything is a widget and nothing is simple"
  :ensure (dart-mode :ref "773e9ebc74a258af2db395b01febfb652a42f3ab")
  :when (gearp! :editing dart)
  :hook
  ((dart-mode-hook dart-ts-mode-hook) . electric-pair-local-mode)
  ((dart-mode-hook dart-ts-mode-hook) .
   (lambda ()
     (toggle-truncate-lines +1)
     (unless (gearp! :editing dart -display-line-numbers)
       (display-line-numbers-mode +1)))))

(leaf dart-ts-mode
  :doc "tree-sitter support for Dart"
  :when (gearp! :editing dart)
  :unless (gearp! :editing dart -treesit)
  :ensure (dart-ts-mode :ref "0dc52bdcf8fb5d6996cbcb67fc41c92986655afe" :host github :repo "50ways2sayhard/dart-ts-mode")
  :after dart-mode)

(leaf eglot
  :doc "Language Server Protocol support for Dart"
  :when (and (gearp! :editing dart) (gearp! :editing dart lsp))
  :doctor ("dart" . ("the Dart SDK, which provides the dart language-server subcommand" required))
  :hook ((dart-mode-hook dart-ts-mode-hook) . eglot-ensure)
  :config
  ;; LSP protocol extensions note: Dart analysis server has non-standard
  ;; extensions for Flutter outline, closing labels, and custom completions.
  ;; Not yet implemented.
  (add-to-list 'eglot-server-programs '(dart-mode . ("dart" "language-server")))
  (add-to-list 'eglot-server-programs '(dart-ts-mode . ("dart" "language-server"))))