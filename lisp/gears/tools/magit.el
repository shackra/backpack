(require 'backpack-pouch)

(leaf transient
  :doc "Installs transient 0.10.1. needed for Magit 4.4.2"
  :when (gearp! :tools magit)
  :ensure (transient :ref "053d56e4de2dd78bf32f7af7ed5f289a91cdb6ac"))

(leaf magit
  :when (gearp! :tools magit)
  :doc "an interface to the version control system Git"
  :bind ("C-x g" . magit-status)
  :ensure (magit :ref "b828afbb4b45641998fb6483a08effb1efb214e1")
  :config
  (leaf forge
    :when (gearp! :tools magit forge)
    :doc "work with Git forges from Magit"
    :ensure (forge :ref "34e7b77602b2018f64bd7e38314a0a2cb4a32227")
    :config
    (setq forge-database-file (expand-file-name "forge-database.sqlite" backpack-cache-dir))))
