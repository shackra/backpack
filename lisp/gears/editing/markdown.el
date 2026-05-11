;; Declare tree-sitter languages needed by this gear.
;; The split_parser branch reorganised the repo so there is no src/ in the
;; root; both markdown and markdown-inline grammars live in subdirectories.
;; markdown-inline is not in treesit-auto's default recipe list at all.
(when (or (backpack-treesit-was-asked 'markdown)
	  ;; Clojure needs markdown inline
	  (backpack-treesit-was-asked 'clojure "30.1"))
  (backpack-treesit-recipe! markdown
    :ts-mode 'markdown-ts-mode
    :remap '(markdown-mode gfm-mode)
    :url "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
    :revision "split_parser"
    :versions ((:until-abi 14 :revision "31c557edb2702e753accdb21c95451d5b9877037"))
    :source-dir "tree-sitter-markdown/src")

  (backpack-treesit-recipe! markdown-inline
    :ts-mode 'markdown-ts-mode
    :url "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
    :revision "split_parser"
    :versions ((:until-abi 14 :revision "31c557edb2702e753accdb21c95451d5b9877037"))
    :source-dir "tree-sitter-markdown-inline/src"))

(leaf markdown-mode
  :doc "the markup language everyone thinks they know until they hit nested lists"
  :when (gearp! :editing markdown)
  :ensure (markdown-mode :ref "9de2df5a9f2f864c82ec112d3369154767a2bb49")
  :mode ("\\.md\\'" "\\.markdown\\'" "\\.mkd\\'")
  :hook
  ((markdown-mode-hook markdown-ts-mode-hook) . visual-line-mode)
  :config
  (when (gearp! :editing markdown display-line-numbers)
    (add-hook 'markdown-mode-hook #'display-line-numbers-mode)
    (add-hook 'markdown-ts-mode-hook #'display-line-numbers-mode)))

(leaf markdown-ts-mode
  :doc "tree-sitter support for Markdown (external, Emacs < 31)"
  :emacs< 31
  :ensure (markdown-ts-mode :ref "2f1ee8b94cdf53cebc31ae08ecfbba846193d5e1")
  :when (and (gearp! :editing markdown)
	     (not (gearp! :editing markdown -treesit)))
  :require t)

(leaf markdown-ts-mode
  :doc "tree-sitter support for Markdown (built-in, Emacs >= 31)"
  :emacs>= 31
  :when (and (gearp! :editing markdown)
	     (not (gearp! :editing markdown -treesit)))
  :require t)
