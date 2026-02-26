(require 'backpack-pouch)

(leaf transient
  :doc "Installs transient 0.12, needed for Magit 4.5"
  :when (gearp! :tools magit)
  :ensure (transient :ref "1f7039ef8d548d6fe858084fcbeae7588eba4190"))

(leaf magit
  :when (gearp! :tools magit)
  :doc "an interface to the version control system Git"
  :bind ("C-x g" . magit-status)
  :ensure (magit :ref "c800f79c2061621fde847f6a53129eca0e8da728")
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
  :ensure (forge :ref "315e8e9a2b45d050ca7fc717595cc698e175b140")
  :config
  (setq forge-database-file (expand-file-name "forge-database.sqlite" backpack-cache-dir)))
