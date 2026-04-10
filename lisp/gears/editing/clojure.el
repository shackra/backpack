;; -*- lexical-binding: t; -*-

;; -- Tree-sitter declaration --
;; Declare tree-sitter grammars needed for Clojure and ClojureScript
;; (when not opted out with -treesit)
(when (and (gearp! :editing clojure)
           (not (gearp! :editing clojure -treesit)))
  (backpack-treesit-langs! clojure))

;; -- Toggle & project helpers --

(defun backpack/clojure-repl-toggle ()
  "Toggle a Clojure REPL buffer.
If a REPL window is visible, delete it.  Otherwise open one."
  (interactive)
  (if-let* ((buf (get-buffer "*clojure-repl*"))
            (win (get-buffer-window buf)))
      (delete-window win)
    (if (get-buffer "*clojure-repl*")
        (pop-to-buffer "*clojure-repl*")
      (cider-jack-in-clojure))))

(defun backpack/clojure-repl-project ()
  "Start Clojure REPL at the current project root."
  (interactive)
  (let ((default-directory (or (project-root (project-current t)) default-directory)))
    (cider-jack-in-clojure)))

;; -- Dispatch function for C-c t p (project terminal) is handled by term gears --
;; Clojure uses C-c C-c for REPL toggle instead

;; -- Display rule: Clojure REPL buffers appear at the bottom --

(when (gearp! :editing clojure)
  )

;; -- Main leaf block --

(leaf clojure-mode
  :doc "Clojure mode for editing Clojure code"
  :when (gearp! :editing clojure)
  :ensure (clojure-mode :ref "c3b039ecf85e343edbc67c5856322654381dbc3e")
  :hook
  ((clojure-mode-hook clojure-ts-mode-hook) . electric-pair-local-mode)
  ((clojure-mode-hook clojure-ts-mode-hook) .
   (lambda ()
     (toggle-truncate-lines +1)
     (unless (gearp! :editing clojure -display-line-numbers)
       (display-line-numbers-mode +1))
     (when (fboundp 'lispy-mode)
       (lispy-mode +1))))
  :bind
  ("C-c C-c" . backpack/clojure-repl-toggle)
  ("C-c C-p" . backpack/clojure-repl-project)
  :custom
  (clojure-indent-style . :align)
  :config
  (leaf eglot
    :doc "Language Server Protocol support for clojure-mode via clojure-lsp"
    :when (gearp! :editing clojure lsp)
    :hook ((clojure-mode-hook clojure-ts-mode-hook) . eglot-ensure)
    :config
    (add-to-list 'eglot-server-programs
                 '(clojure-mode . ("clojure-lsp")))
    (add-to-list 'eglot-server-programs
                 '(clojure-ts-mode . ("clojure-lsp"))))
  :doctor
  ("clojure-lsp" . ("official Language Server Protocol implementation for Clojure" required))
  ;; Add clojure-lsp to the PATH if needed via environment
  :config
  (add-to-list 'display-buffer-alist
               '("\\*clojure-repl"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.35))))

;; REPL support via CIDER
(leaf cider
  :doc "Clojure Interactive Development Environment that Really Rocks"
  :when (gearp! :editing clojure repl)
  :ensure (cider :ref "5d003b579c7a7b55321c6e4e672f0f57b4021930")
  :doctor
  ("nrepl-server" . ("REPL server for Clojure" required))
  :config
  ;; Enable pretty printing and other CIDER goodies
  (setq cider-repl-use-clojure-font-lock t)
  (setq cider-repl-history-file (expand-file-name "cider-repl-history" backpack-cache-dir))
  ;; Use projective stacktraces for better readability
  (setq cider-stacktrace-fn #'cider-trace-basics-mode)
  (setq cider-show-error-buffer t))

;; Org-mode Babel support for Clojure
(leaf ob-clojure
  :doc "Clojure source blocks in org-mode"
  :when (and (gearp! :editing clojure) (gearp! :editing org))
  :after org
  :ensure (ob-clojure :ref "be059d231fafeb24a658db212a55ccdc55c0c500")
  :doctor
  ("clojure" . ("the Clojure compiler/interpreter" required))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   (append org-babel-load-languages '((clojure . t)))))
