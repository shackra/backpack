(require 'backpack-pouch)

(leaf theme
  :doc "Emacs theme configuration"
  :tag "ui" "theme"
  :config ;; TODO(shackra): Indicate in theme that only one flag/feature can be chosen
  (leaf doom-one
    :when (gearp! :ui theme doom-one)
    :doc "inspired by Atom One Dark (ported by @hlissner)"
    :ensure (doom-one :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-one-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-one t))

  (leaf doom-1337
    :when (gearp! :ui theme doom-1337)
    :doc "ported from VSCode's 1337 theme (ported by @ccmywish)"
    :ensure (doom-1337 :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-1337-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-1337 t))

  (leaf doom-acario-dark
    :when (gearp! :ui theme doom-acario-dark)
    :doc "an original dark theme (ported by @gagbo)"
    :ensure (doom-acario-dark :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-acario-dark-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-acario-dark t))

  (leaf doom-acario-light
    :when (gearp! :ui theme doom-acario-light)
    :doc "an original light theme (ported by @gagbo)"
    :ensure (doom-acario-light :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-acario-light-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-acario-light t))

  (leaf doom-ayu-dark
    :when (gearp! :ui theme doom-ayu-dark)
    :doc "inspired by Ayu Dark (ported by @ashton)"
    :ensure (doom-ayu-dark :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-ayu-dark-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-ayu-dark t))

  (leaf doom-ayu-light
    :when (gearp! :ui theme doom-ayu-light)
    :doc "inspirted by Ayu Light (ported by @LoveSponge)"
    :ensure (doom-ayu-light :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-ayu-light-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-ayu-light t))

  (leaf doom-ayu-mirage
    :when (gearp! :ui theme doom-ayu-mirage)
    :doc "inspired by Ayu Mirage (ported by @LoveSponge)"
    :ensure (doom-ayu-mirage :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-ayu-mirage-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-ayu-mirage t))

  (leaf doom-badger
    :when (gearp! :ui theme doom-badger)
    :doc "inspired by cann's Badger colorscheme (ported by @jsoa)"
    :ensure (doom-badger :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-badger-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-badger t))

  (leaf doom-challenger-deep
    :when (gearp! :ui theme doom-challenger-deep)
    :doc "inspired by Vim's Challenger Deep theme (ported by @fuxialexander)"
    :ensure (doom-challenger-deep :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-challenger-deep-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-challenger-deep t))

  (leaf doom-city-lights
    :when (gearp! :ui theme doom-city-lights)
    :doc "inspired by Atom's City Lights theme (ported by @fuxialexander)"
    :ensure (doom-city-lights :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-city-lights-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-city-lights t))

  (leaf doom-dark+
    :when (gearp! :ui theme doom-dark+)
    :doc "ported from equinusocio's VSCode Theme, dark+ (ported by @ema2159)"
    :ensure (doom-dark+ :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-dark+-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-dark+ t))

  (leaf doom-dracula
    :when (gearp! :ui theme doom-dracula)
    :doc "inspired by the popular Dracula theme (ported by @fuxialexander)"
    :ensure (doom-dracula :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-dracula-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-dracula t))

  (leaf doom-earl-grey
    :when (gearp! :ui theme doom-earl-grey)
    :doc "a gentle color scheme, for code (ported by @JuneKelly)"
    :ensure (doom-earl-grey :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-earl-grey-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-earl-grey t))

  (leaf doom-ephemeral
    :when (gearp! :ui theme doom-ephemeral)
    :doc "inspired by the Ephemeral Theme from elenapan's dotfiles (ported by @karetsu)"
    :ensure (doom-ephemeral :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-ephemeral-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-ephemeral t))

  (leaf doom-fairy-floss
    :when (gearp! :ui theme doom-fairy-floss)
    :doc "a candy colored theme by sailorhg (ported by @ema2159)"
    :ensure (doom-fairy-floss :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-fairy-floss-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-fairy-floss t))

  (leaf doom-feather-dark
    :when (gearp! :ui theme doom-feather-dark)
    :doc "a purple-tinted theme, inspired by doom-one (by @Plunne)"
    :ensure (doom-feather-dark :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-feather-dark-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-feather-dark t))

  (leaf doom-feather-light
    :when (gearp! :ui theme doom-feather-light)
    :doc "a light variable of feather-dark, inspired by doom-one (by @Plunne)"
    :ensure (doom-feather-light :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-feather-light-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-feather-light t))

  (leaf doom-flatwhite
    :when (gearp! :ui theme doom-flatwhite)
    :doc "inspired by Atom's Flatwhite Syntax theme (ported by @JuneKelly)"
    :ensure (doom-flatwhite :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-flatwhite-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-flatwhite t))

  (leaf doom-gruvbox
    :when (gearp! :ui theme doom-gruvbox)
    :doc "inspired by morhetz's Gruvbox (ported by @JongW)"
    :ensure (doom-gruvbox :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-gruvbox-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-gruvbox t))

  (leaf doom-gruvbox-light
    :when (gearp! :ui theme doom-gruvbox-light)
    :doc "inspired by morhetz's Gruvbox (light) (ported by @jsoa)"
    :ensure (doom-gruvbox-light :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-gruvbox-light-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-gruvbox-light t))

  (leaf doom-henna
    :when (gearp! :ui theme doom-henna)
    :doc "based on VSCode's Henna theme (ported by @jsoa)"
    :ensure (doom-henna :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-henna-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-henna t))

  (leaf doom-homage-black
    :when (gearp! :ui theme doom-homage-black)
    :doc "a minimalistic, colorless theme inspired by eziam, tao, and jbeans (ported by @mskorzhinskiy)"
    :ensure (doom-homage-black :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-homage-black-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-homage-black t))

  (leaf doom-homage-white
    :when (gearp! :ui theme doom-homage-white)
    :doc "minimal white theme inspired by editors from 2000s (ported by @mskorzhinskiy)"
    :ensure (doom-homage-white :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-homage-white-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-homage-white t))

  (leaf doom-horizon
    :when (gearp! :ui theme doom-horizon)
    :doc "ported from VSCode Horizon (ported by @karetsu)"
    :ensure (doom-horizon :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-horizon-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-horizon t))

  (leaf doom-Iosvkem
    :when (gearp! :ui theme doom-Iosvkem)
    :doc "ported from the default dark theme for Adobe Brackets (ported by @neutaaaaan)"
    :ensure (doom-Iosvkem :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-Iosvkem-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-Iosvkem t))

  (leaf doom-ir-black
    :when (gearp! :ui theme doom-ir-black)
    :doc "ported from Vim's ir_black colorscheme (ported by @legendre6891)"
    :ensure (doom-ir-black :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-ir-black-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-ir-black t))

  (leaf doom-lantern
    :when (gearp! :ui theme doom-lantern)
    :doc "based on Gitleptune's Lantern theme (ported by @paladhammika)"
    :ensure (doom-lantern :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-lantern-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-lantern t))

  (leaf doom-laserwave
    :when (gearp! :ui theme doom-laserwave)
    :doc "a clean synthwave/outrun theme inspired by VSCode's Laserwave (ported by @hyakt)"
    :ensure (doom-laserwave :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-laserwave-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-laserwave t))

  (leaf doom-manegarm
    :when (gearp! :ui theme doom-manegarm)
    :doc "an original autumn-inspired dark theme (ported by @kenranunderscore)"
    :ensure (doom-manegarm :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-manegarm-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-managarm t))

  (leaf doom-material
    :when (gearp! :ui theme doom-material)
    :doc "adapted from equinusocio's Material themes (ported by @tam5)"
    :ensure (doom-material :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-material-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-material t))

  (leaf doom-material-dark
    :when (gearp! :ui theme doom-material-dark)
    :doc "inspired by Material Theme by xrei (ported by @trev-dev)"
    :ensure (doom-material-dark :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-material-dark-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-material-dark t))

  (leaf doom-meltbus
    :when (gearp! :ui theme doom-meltbus)
    :doc "a dark (mostly) monochromatic theme (ported by @spacefrogg)"
    :ensure (doom-meltbus :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-meltbus-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-meltbus t))

  (leaf doom-miramare
    :when (gearp! :ui theme doom-miramare)
    :doc "a port of Franbach's Miramare theme; a variant of Grubox (ported by @sagittaros)"
    :ensure (doom-miramare :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-miramare-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-miramare t))

  (leaf doom-molokai
    :when (gearp! :ui theme doom-molokai)
    :doc "inspired by Tomas Restrepo's Molokai (ported by @hlissner)"
    :ensure (doom-molokai :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-molokai-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-molokai t))

  (leaf doom-monokai-classic
    :when (gearp! :ui theme doom-monokai-classic)
    :doc "port of Monokai Classic (ported by @ema2159)"
    :ensure (doom-monokai-classic :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-monokai-classic-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-monokai-classic t))

  (leaf doom-monokai-machine
    :when (gearp! :ui theme doom-monokai-machine)
    :doc "port of Monokai Classic (ported by @minikN)"
    :ensure (doom-monokai-machine :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-monokai-machine-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-monokai-machine t))

  (leaf doom-monokai-octagon
    :when (gearp! :ui theme doom-monokai-octagon)
    :doc "port of Monokai Octagon (ported by @minikN)"
    :ensure (doom-monokai-octagon :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-monokai-octagon-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-monokai-octagon t))

  (leaf doom-monokai-pro
    :when (gearp! :ui theme doom-monokai-pro)
    :doc "Port of Monokai Pro (ported by @minikN)"
    :ensure (doom-monokai-pro :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-monokai-pro-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-monokai-pro t))

  (leaf doom-monokai-ristretto
    :when (gearp! :ui theme doom-monokai-ristretto)
    :doc "Port of Monokai Ristretto (ported by @minikN)"
    :ensure (doom-monokai-ristretto :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-monokai-ristretto-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-monokai-ristretto t))

  (leaf doom-monokai-spectrum
    :when (gearp! :ui theme doom-monokai-spectrum)
    :doc "port of Monokai Spectrum (ported by @minikN)"
    :ensure (doom-monokai-spectrum :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-monokai-spectrum-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-monokai-spectrum t))

  (leaf doom-moonlight
    :when (gearp! :ui theme doom-moonlight)
    :doc "inspired by VS code's Moonlight (ported by @Brettm12345)"
    :ensure (doom-moonlight :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-moonlight-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-moonlight t))

  (leaf doom-nord
    :when (gearp! :ui theme doom-nord)
    :doc "dark variant of Nord (ported by @fuxialexander)"
    :ensure (doom-nord :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-nord-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-nord t))

  (leaf doom-nord-aurora
    :when (gearp! :ui theme doom-nord-aurora)
    :doc "a light variant of Nord (ported by @MoskitoHero)"
    :ensure (doom-nord-aurora :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-nord-aurora-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-nord-aurora t))

  (leaf doom-nord-light
    :when (gearp! :ui theme doom-nord-light)
    :doc "light variant of Nord (ported by @fuxialexander)"
    :ensure (doom-nord-light :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-nord-light-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-nord-light t))

  (leaf doom-nova
    :when (gearp! :ui theme doom-nova)
    :doc "inspired by Trevord Miller's Nova (ported by @bigardone)"
    :ensure (doom-nova :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-nova-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-nova t))

  (leaf doom-oceanic-next
    :when (gearp! :ui theme doom-oceanic-next)
    :doc "inspired by Oceanic Next (ported by @juanwolf)"
    :ensure (doom-oceanic-next :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-oceanic-next-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-oceanic-next t))

  (leaf doom-oksolar-dark
    :when (gearp! :ui theme doom-oksolar-dark)
    :doc "an OKLab variant of Solarized dark (ported by @logc)"
    :ensure (doom-oksolar-dark :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-oksolar-dark-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-oksolar-dark t))

  (leaf doom-oksolar-light
    :when (gearp! :ui theme doom-oksolar-light)
    :doc "an OKLab variant of Solarized light (ported by @logc)"
    :ensure (doom-oksolar-light :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-oksolar-light-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-oksolar-light t))

  (leaf doom-old-hope
    :when (gearp! :ui theme doom-old-hope)
    :doc "inspired by An Old Hope, in a galaxy far far away (ported by @teesloane)"
    :ensure (doom-old-hope :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-old-hope-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-old-hope t))

  (leaf doom-one-light
    :when (gearp! :ui theme doom-one-light)
    :doc "inspired by Atom One Light (ported by @ztlevi)"
    :ensure (doom-one-light :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-one-light-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-one-light t))

  (leaf doom-opera
    :when (gearp! :ui theme doom-opera)
    :doc "an original light theme (ported by @jwintz)"
    :ensure (doom-opera :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-opera-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-opera t))

  (leaf doom-opera-light
    :when (gearp! :ui theme doom-opera-light)
    :doc "an original light theme (ported by @jwintz)"
    :ensure (doom-opera-light :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-opera-light-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-opera-light t))

  (leaf doom-outrun-electric
    :when (gearp! :ui theme doom-outrun-electric)
    :doc "a high contrast, neon theme inspired by Outrun Electric on VSCode (ported by @ema2159)"
    :ensure (doom-outrun-electric :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-outrun-electric-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-outrun-electric t))

  (leaf doom-palenight
    :when (gearp! :ui theme doom-palenight)
    :doc "adapted from equinusocio's Material themes (ported by @Brettm12345)"
    :ensure (doom-palenight :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-palenight-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-palenight t))

  (leaf doom-peacock
    :when (gearp! :ui theme doom-peacock)
    :doc "inspired by daylerees' Peacock (ported by @teesloane)"
    :ensure (doom-peacock :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-peacock-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-peacock t))

  (leaf doom-pine
    :when (gearp! :ui theme doom-pine)
    :doc "a green flavor of doom-gruvbox (by @RomanHargrave)"
    :ensure (doom-pine :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-pine-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-pine t))

  (leaf doom-plain
    :when (gearp! :ui theme doom-plain)
    :doc "inspired by gko's plain theme for VSCode (ported by @das-s)"
    :ensure (doom-plain :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-plain-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-plain t))

  (leaf doom-plain-dark
    :when (gearp! :ui theme doom-plain-dark)
    :doc "inspired by gko's plain theme for VSCode (ported by @das-s)"
    :ensure (doom-plain-dark :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-plain-dark-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-plain-dark t))

  (leaf doom-rouge
    :when (gearp! :ui theme doom-rouge)
    :doc "ported from VSCode's Rouge Theme (ported by @das-s)"
    :ensure (doom-rouge :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-rouge-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-rouge t))

  (leaf doom-shades-of-purple
    :when (gearp! :ui theme doom-shades-of-purple)
    :doc "a port of VSCode's Shades of Purple (ported by @jwbaldwin)"
    :ensure (doom-shades-of-purple :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-shades-of-purple-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-shades-of-purple t))

  (leaf doom-snazzy
    :when (gearp! :ui theme doom-snazzy)
    :doc "inspired by Hyper Snazzy (ported by @ar1a)"
    :ensure (doom-snazzy :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-snazzy-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-snazzy t))

  (leaf doom-solarized-dark
    :when (gearp! :ui theme doom-solarized-dark)
    :doc "a dark variant of Solarized (ported by @ema2159)"
    :ensure (doom-solarized-dark :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-solarized-dark-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-solarized-dark t))

  (leaf doom-solarized-dark-high-contrast
    :when (gearp! :ui theme doom-solarized-dark-high-contrast)
    :doc "a high-contrast variant of Solarized Dark (ported by @jmorag)"
    :ensure (doom-solarized-dark-high-contrast :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-solarized-dark-high-contrast-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-solarized-dark-high-contrast t))

  (leaf doom-solarized-light
    :when (gearp! :ui theme doom-solarized-light)
    :doc "a light variant of Solarized (ported by @fuxialexander)"
    :ensure (doom-solarized-light :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-solarized-light-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-solarized-light t))

  (leaf doom-sourcerer
    :when (gearp! :ui theme doom-sourcerer)
    :doc "a port of xero's Sourcerer (ported by @fm0xb)"
    :ensure (doom-sourcerer :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-sourcerer-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-sourcerer t))

  (leaf doom-spacegrey
    :when (gearp! :ui theme doom-spacegrey)
    :doc "I'm sure you've heard of it (ported by @teesloane)"
    :ensure (doom-spacegrey :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-spacegrey-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-spacegrey t))

  (leaf doom-tokyo-night
    :when (gearp! :ui theme doom-tokyo-night)
    :doc "inspired by VSCode's Tokyo Night theme (ported by @FosterHangdaan)"
    :ensure (doom-tokyo-night :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-tokyo-night-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-tokyo-night t))

  (leaf doom-tomorrow-day
    :when (gearp! :ui theme doom-tomorrow-day)
    :doc "a light variant of Tomorrow (ported by @emacswatcher)"
    :ensure (doom-tomorrow-day :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-tomorrow-day-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-tomorrow-day t))

  (leaf doom-tomorrow-night
    :when (gearp! :ui theme doom-tomorrow-night)
    :doc "One of the dark variants of Tomorrow (ported by @hlissner)"
    :ensure (doom-tomorrow-night :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-tomorrow-night-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-tomorrow-night t))

  (leaf doom-vibrant
    :when (gearp! :ui theme doom-vibrant)
    :doc "a more vibrant variant of doom-one (ported by @hlissner)"
    :ensure (doom-vibrant :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-vibrant-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-vibrant t))

  (leaf doom-wilmersdorf
    :when (gearp! :ui theme doom-wilmersdorf)
    :doc "port of Ian Pan's Wilmersdorf (ported by @ema2159)"
    :ensure (doom-wilmersdorf :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-wilmersdorf-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-wilmersdorf t))

  (leaf doom-xcode
    :when (gearp! :ui theme doom-xcode)
    :doc "based off of Apple's Xcode Dark Theme (ported by @kadenbarlow)"
    :ensure (doom-xcode :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-xcode-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-xcode t))

  (leaf doom-zenburn
    :when (gearp! :ui theme doom-zenburn)
    :doc "port of the popular Zenburn theme (ported by @jsoa)"
    :ensure (doom-zenburn :host github :repo "doomemacs/themes" :ref "556598955c67540eac8811835b327f299ffb58c7" :files ("doom-themes.el" "doom-themes-base.el" "themes/doom-zenburn-theme.el"))
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-zenburn t)))
