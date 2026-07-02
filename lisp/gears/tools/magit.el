(leaf transient
  :doc "Transient command dispatching (needed by Magit and diff-hl menu)"
  :when (or (gearp! :tools magit)
            (gearp! :ui diff-hl))
  :ensure (transient :ref "3d20a780605f0a33d6360dc0a2ce9174c69a9a92"))

(leaf magit
  :when (gearp! :tools magit)
  :doc "an interface to the version control system Git"
  :bind ("C-x g" . magit-status)
  :ensure (magit :ref "b6c512597fd66abe69883a058a2d13bcea76bf33")
  :custom
  ;; Show magit-status in the entire frame; other magit buffers use
  ;; the traditional split behaviour.  The pre-display-buffer-hook
  ;; already contains `magit-save-window-configuration' by default,
  ;; so the previous layout is recorded automatically.
  (magit-display-buffer-function . #'magit-display-buffer-fullframe-status-v1)
  ;; Restore the saved window configuration when quitting magit (default).
  (magit-bury-buffer-function . #'magit-restore-window-configuration))

(leaf forge
  :when (gearp! :tools magit forge)
  :doc "work with Git forges from Magit"
  :ensure (forge :ref "9628f76740aec9270e9fb31457ff4cb38d9f3f16")
  :config
  (setq forge-database-file (expand-file-name "forge-database.sqlite" backpack-cache-dir)))

(leaf magit-difftastic
  :when (gearp! :tools magit difftastic)
  :doc "difftastic integration for Magit"
  :ensure (magit-difftastic :host github :repo "rschmukler/magit-difftastic" :ref "0df64c67ba4b73cca705f72f7357aedce82f8529")
  :doctor (executable-find "difft")
  :global-minor-mode magit-difftastic-mode)

;; Register magit-project-status in project-switch-commands eagerly
;; so it appears in C-x p p dispatch before magit is loaded.
;; magit-extras.el does this after loading, but we want it available
;; immediately.  Its guard clause prevents duplication when magit
;; later loads.
(when (gearp! :tools magit)
  (autoload 'magit-project-status "magit-extras"
    "Run `magit-status' in the current project's root." t)
  (with-eval-after-load 'project
    (keymap-set project-prefix-map "m" #'magit-project-status)
    (add-to-list 'project-switch-commands '(magit-project-status "Magit") t)))
