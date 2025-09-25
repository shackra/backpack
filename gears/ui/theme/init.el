(leaf doom-themes
  :ensure (doom-themes
	   :ref "556598955c67540eac8811835b327f299ffb58c7")
  :custom
  (doom-themes-enable-bold . t)
  (doom-themes-enable-italic . t)
  :config
  (load-theme 'doom-one t))
