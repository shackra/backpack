;; -*- lexical-binding: t; -*-

(leaf anvil
  :doc "Emacs MCP server — Forge Emacs into an AI’s Weapon"
  :url "https://github.com/zawatton/anvil.el"
  :when (gearp! :ai anvil)
  :ensure (anvil :host github :repo "zawatton/anvil.el"
                 :ref "b65dc7c738e09fdbc7a9c8d57ad512a5af4d7c4d"
                 :files ("anvil*.el" "anvil-stdio.sh"))
  :hook (backpack-user-after-init-hook . (lambda ()
					   (unless (file-exists-p (expand-file-name "anvil-stdio.sh" anvil-server-install-directory))
					     (anvil-server-install))
					   (anvil-enable)
					   (anvil-server-start)))
  :setq
  (anvil-modules . '(worker eval org file host git proc fs emacs text clipboard data net))
  :custom
  (anvil-server-install-directory . `,(expand-file-name "anvil" backpack-nonessential-dir))
  :config
  (setq anvil-optional-modules
        (append
         (when (gearp! :ai anvil ide)     '(ide))
         (when (gearp! :ai anvil state)   '(sqlite org-index))
         (when (gearp! :ai anvil cron)    '(cron))
         (when (gearp! :ai anvil http)    '(http sqlite org-index))
         (when (gearp! :ai anvil browser) '(browser)))))
