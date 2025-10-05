(require 'backpack-pouch)

(leaf theme
  :doc "Emacs theme configuration"
  :tag "ui" "theme"
  :config ;; TODO(shackra): Indicate in theme that only one flag/feature can be chosen
  (leaf doom-one
    :disabled (gear-keep-disabledp! :ui theme doom-one)
    :doc "inspired by Atom One Dark (ported by @hlissner)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-one t))

  (leaf doom-1337
    :disabled (gear-keep-disabledp! :ui theme doom-1337)
    :doc "ported from VSCode's 1337 theme (ported by @ccmywish)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-1337 t))

  (leaf doom-acario-dark
    :disabled (gear-keep-disabledp! :ui theme doom-acario-dark)
    :doc "an original dark theme (ported by @gagbo)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-acario-dark t))

  (leaf doom-acario-ligh (gear-keep-disabledp! :ui theme doom-acario-light)
    :disabled t
    :doc "an original light theme (ported by @gagbo)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-acario-light t))

  (leaf doom-ayu-dark
    :disabled (gear-keep-disabledp! :ui theme doom-ayu-dark)
    :doc "inspired by Ayu Dark (ported by @ashton)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-ayu-dark t))

  (leaf doom-ayu-light
    :disabled (gear-keep-disabledp! :ui theme doom-ayu-light)
    :doc "inspirted by Ayu Light (ported by @LoveSponge)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-ayu-light t))

  (leaf doom-ayu-mirage
    :disabled (gear-keep-disabledp! :ui theme doom-ayu-mirage)
    :doc "inspired by Ayu Mirage (ported by @LoveSponge)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-ayu-mirage t))

  (leaf doom-badger
    :disabled (gear-keep-disabledp! :ui theme doom-badger)
    :doc "inspired by cann's Badger colorscheme (ported by @jsoa)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-badger t))

  (leaf doom-challenger-deep
    :disabled (gear-keep-disabledp! :ui theme doom-challenger-deep)
    :doc "inspired by Vim's Challenger Deep theme (ported by @fuxialexander)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-challenger-deep t))

  (leaf doom-city-lights
    :disabled (gear-keep-disabledp! :ui theme doom-city-lights)
    :doc "inspired by Atom's City Lights theme (ported by @fuxialexander)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-city-lights t))

  (leaf doom-dark+
    :disabled (gear-keep-disabledp! :ui theme doom-dark+)
    :doc "ported from equinusocio's VSCode Theme, dark+ (ported by @ema2159)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-dark+ t))

  (leaf doom-dracula
    :disabled (gear-keep-disabledp! :ui theme doom-dracula)
    :doc "inspired by the popular Dracula theme (ported by @fuxialexander)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-dracula t))

  (leaf doom-earl-grey
    :disabled (gear-keep-disabledp! :ui theme doom-earl-grey)
    :doc "a gentle color scheme, for code (ported by @JuneKelly)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-earl-grey t))

  (leaf doom-ephemeral
    :disabled (gear-keep-disabledp! :ui theme doom-ephemeral)
    :doc "inspired by the Ephemeral Theme from elenapan's dotfiles (ported by @karetsu)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-ephemeral t))

  (leaf doom-fairy-floss
    :disabled (gear-keep-disabledp! :ui theme doom-fairy-floss)
    :doc "a candy colored theme by sailorhg (ported by @ema2159)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-fairy-floss t))

  (leaf doom-feather-dark
    :disabled (gear-keep-disabledp! :ui theme doom-feather-dark)
    :doc "a purple-tinted theme, inspired by doom-one (by @Plunne)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-feather-dark t))

  (leaf doom-feather-light
    :disabled (gear-keep-disabledp! :ui theme doom-feather-light)
    :doc "a light variable of feather-dark, inspired by doom-one (by @Plunne)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-feather-light t))

  (leaf doom-flatwhite
    :disabled (gear-keep-disabledp! :ui theme doom-flatwhite)
    :doc "inspired by Atom's Flatwhite Syntax theme (ported by @JuneKelly)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-flatwhite t))

  (leaf doom-gruvbox
    :disabled (gear-keep-disabledp! :ui theme doom-gruvbox)
    :doc "inspired by morhetz's Gruvbox (ported by @JongW)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-gruvbox t))

  (leaf doom-gruvbox-light
    :disabled (gear-keep-disabledp! :ui theme doom-gruvbox-light)
    :doc "inspired by morhetz's Gruvbox (light) (ported by @jsoa)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-gruvbox-light t))

  (leaf doom-henna
    :disabled (gear-keep-disabledp! :ui theme doom-henna)
    :doc "based on VSCode's Henna theme (ported by @jsoa)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-henna t))

  (leaf doom-homage-black
    :disabled (gear-keep-disabledp! :ui theme doom-homage-black)
    :doc "a minimalistic, colorless theme inspired by eziam, tao, and jbeans (ported by @mskorzhinskiy)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-homage-black t))

  (leaf doom-homage-white
    :disabled (gear-keep-disabledp! :ui theme doom-homage-white)
    :doc "minimal white theme inspired by editors from 2000s (ported by @mskorzhinskiy)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-homage-white t))

  (leaf doom-horizon
    :disabled (gear-keep-disabledp! :ui theme doom-horizon)
    :doc "ported from VSCode Horizon (ported by @karetsu)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-horizon t))

  (leaf doom-Iosvkem
    :disabled (gear-keep-disabledp! :ui theme doom-Iosvkem)
    :doc "ported from the default dark theme for Adobe Brackets (ported by @neutaaaaan)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-Iosvkem t))

  (leaf doom-ir-black
    :disabled (gear-keep-disabledp! :ui theme doom-ir-black)
    :doc "ported from Vim's ir_black colorscheme (ported by @legendre6891)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-ir-black t))

  (leaf doom-lantern
    :disabled (gear-keep-disabledp! :ui theme doom-lantern)
    :doc "based on Gitleptune's Lantern theme (ported by @paladhammika)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-lantern t))

  (leaf doom-laserwave
    :disabled (gear-keep-disabledp! :ui theme doom-laserwave)
    :doc "a clean synthwave/outrun theme inspired by VSCode's Laserwave (ported by @hyakt)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-laserwave t))

  (leaf doom-manegarm
    :disabled (gear-keep-disabledp! :ui theme doom-manegarm)
    :doc "an original autumn-inspired dark theme (ported by @kenranunderscore)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-managarm t))

  (leaf doom-material
    :disabled (gear-keep-disabledp! :ui theme doom-material)
    :doc "adapted from equinusocio's Material themes (ported by @tam5)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-material t))

  (leaf doom-material-dark
    :disabled (gear-keep-disabledp! :ui theme doom-material-dark)
    :doc "inspired by Material Theme by xrei (ported by @trev-dev)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-material-dark t))

  (leaf doom-meltbus
    :disabled (gear-keep-disabledp! :ui theme doom-meltbus)
    :doc "a dark (mostly) monochromatic theme (ported by @spacefrogg)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-meltbus t))

  (leaf doom-miramare
    :disabled (gear-keep-disabledp! :ui theme doom-miramare)
    :doc "a port of Franbach's Miramare theme; a variant of Grubox (ported by @sagittaros)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-miramare t))

  (leaf doom-molokai
    :disabled (gear-keep-disabledp! :ui theme doom-molokai)
    :doc "inspired by Tomas Restrepo's Molokai (ported by @hlissner)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-molokai t))

  (leaf doom-monokai-classic
    :disabled (gear-keep-disabledp! :ui theme doom-monokai-classic)
    :doc "port of Monokai Classic (ported by @ema2159)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-monokai-classic t))

  (leaf doom-monokai-machine
    :disabled (gear-keep-disabledp! :ui theme doom-monokai-machine)
    :doc "port of Monokai Classic (ported by @minikN)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-monokai-machine t))

  (leaf doom-monokai-octagon
    :disabled (gear-keep-disabledp! :ui theme doom-monokai-octagon)
    :doc "port of Monokai Octagon (ported by @minikN)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-monokai-octagon t))

  (leaf doom-monokai-pro
    :disabled (gear-keep-disabledp! :ui theme doom-monokai-pro)
    :doc "Port of Monokai Pro (ported by @minikN)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-monokai-pro t))

  (leaf doom-monokai-ristretto
    :disabled (gear-keep-disabledp! :ui theme doom-monokai-ristretto)
    :doc "Port of Monokai Ristretto (ported by @minikN)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-monokai-ristretto t))

  (leaf doom-monokai-spectrum
    :disabled (gear-keep-disabledp! :ui theme doom-monokai-spectrum)
    :doc "port of Monokai Spectrum (ported by @minikN)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-monokai-spectrum t))

  (leaf doom-moonlight
    :disabled (gear-keep-disabledp! :ui theme doom-moonlight)
    :doc "inspired by VS code's Moonlight (ported by @Brettm12345)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-moonlight t))

  (leaf doom-nord
    :disabled (gear-keep-disabledp! :ui theme doom-nord)
    :doc "dark variant of Nord (ported by @fuxialexander)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-nord t))

  (leaf doom-nord-aurora
    :disabled (gear-keep-disabledp! :ui theme doom-nord-aurora)
    :doc "a light variant of Nord (ported by @MoskitoHero)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-nord-aurora t))

  (leaf doom-nord-light
    :disabled (gear-keep-disabledp! :ui theme doom-nord-light)
    :doc "light variant of Nord (ported by @fuxialexander)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-nord-light t))

  (leaf doom-nova
    :disabled (gear-keep-disabledp! :ui theme doom-nova)
    :doc "inspired by Trevord Miller's Nova (ported by @bigardone)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-nova t))

  (leaf doom-oceanic-next
    :disabled (gear-keep-disabledp! :ui theme doom-oceanic-next)
    :doc "inspired by Oceanic Next (ported by @juanwolf)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-oceanic-next t))

  (leaf doom-oksolar-dark
    :disabled (gear-keep-disabledp! :ui theme doom-oksolar-dark)
    :doc "an OKLab variant of Solarized dark (ported by @logc)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-oksolar-dark t))

  (leaf doom-oksolar-light
    :disabled (gear-keep-disabledp! :ui theme doom-oksolar-light)
    :doc "an OKLab variant of Solarized light (ported by @logc)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-oksolar-light t))

  (leaf doom-old-hope
    :disabled (gear-keep-disabledp! :ui theme doom-old-hope)
    :doc "inspired by An Old Hope, in a galaxy far far away (ported by @teesloane)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-old-hope t))

  (leaf doom-one-light
    :disabled (gear-keep-disabledp! :ui theme doom-one-light)
    :doc "inspired by Atom One Light (ported by @ztlevi)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-one-light t))

  (leaf doom-opera
    :disabled (gear-keep-disabledp! :ui theme doom-opera)
    :doc "an original light theme (ported by @jwintz)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-opera t))

  (leaf doom-opera-light
    :disabled (gear-keep-disabledp! :ui theme doom-opera-light)
    :doc "an original light theme (ported by @jwintz)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-opera-light t))

  (leaf doom-outrun-electric
    :disabled (gear-keep-disabledp! :ui theme doom-outrun-electric)
    :doc "a high contrast, neon theme inspired by Outrun Electric on VSCode (ported by @ema2159)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-outrun-electric t))

  (leaf doom-palenight
    :disabled (gear-keep-disabledp! :ui theme doom-palenight)
    :doc "adapted from equinusocio's Material themes (ported by @Brettm12345)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-palenight t))

  (leaf doom-peacock
    :disabled (gear-keep-disabledp! :ui theme doom-peacock)
    :doc "inspired by daylerees' Peacock (ported by @teesloane)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-peacock t))

  (leaf doom-pine
    :disabled (gear-keep-disabledp! :ui theme doom-pine)
    :doc "a green flavor of doom-gruvbox (by @RomanHargrave)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-pine t))

  (leaf doom-plain
    :disabled (gear-keep-disabledp! :ui theme doom-plain)
    :doc "inspired by gko's plain theme for VSCode (ported by @das-s)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-plain t))

  (leaf doom-plain-dark
    :disabled (gear-keep-disabledp! :ui theme doom-plain-dark)
    :doc "inspired by gko's plain theme for VSCode (ported by @das-s)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-plain-dark t))

  (leaf doom-rouge
    :disabled (gear-keep-disabledp! :ui theme doom-rouge)
    :doc "ported from VSCode's Rouge Theme (ported by @das-s)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-rouge t))

  (leaf doom-shades-of-purple
    :disabled (gear-keep-disabledp! :ui theme doom-shades-of-purple)
    :doc "a port of VSCode's Shades of Purple (ported by @jwbaldwin)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-shades-of-purple t))

  (leaf doom-snazzy
    :disabled (gear-keep-disabledp! :ui theme doom-snazzy)
    :doc "inspired by Hyper Snazzy (ported by @ar1a)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-snazzy t))

  (leaf doom-solarized-dark
    :disabled (gear-keep-disabledp! :ui theme doom-solarized-dark)
    :doc "a dark variant of Solarized (ported by @ema2159)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-solarized-dark t))

  (leaf doom-solarized-dark-high-contrast
    :disabled (gear-keep-disabledp! :ui theme doom-solarized-dark-high-contrast)
    :doc "a high-contrast variant of Solarized Dark (ported by @jmorag)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-solarized-dark-high-contrast t))

  (leaf doom-solarized-light
    :disabled (gear-keep-disabledp! :ui theme doom-solarized-light)
    :doc "a light variant of Solarized (ported by @fuxialexander)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-solarized-light t))

  (leaf doom-sourcerer
    :disabled (gear-keep-disabledp! :ui theme doom-sourcerer)
    :doc "a port of xero's Sourcerer (ported by @fm0xb)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-sourcerer t))

  (leaf doom-spacegrey
    :disabled (gear-keep-disabledp! :ui theme doom-spacegrey)
    :doc "I'm sure you've heard of it (ported by @teesloane)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-spacegrey t))

  (leaf doom-tokyo-night
    :disabled (gear-keep-disabledp! :ui theme doom-tokyo-night)
    :doc "inspired by VSCode's Tokyo Night theme (ported by @FosterHangdaan)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-tokyo-night t))

  (leaf doom-tomorrow-day
    :disabled (gear-keep-disabledp! :ui theme doom-tomorrow-day)
    :doc "a light variant of Tomorrow (ported by @emacswatcher)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-tomorrow-day t))

  (leaf doom-tomorrow-night
    :disabled (gear-keep-disabledp! :ui theme doom-tomorrow-night)
    :doc "One of the dark variants of Tomorrow (ported by @hlissner)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-tomorrow-night t))

  (leaf doom-vibrant
    :disabled (gear-keep-disabledp! :ui theme doom-vibrant)
    :doc "a more vibrant variant of doom-one (ported by @hlissner)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-vibrant t))

  (leaf doom-wilmersdorf
    :disabled (gear-keep-disabledp! :ui theme doom-wilmersdorf)
    :doc "port of Ian Pan's Wilmersdorf (ported by @ema2159)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-wilmersdorf t))

  (leaf doom-xcode
    :disabled (gear-keep-disabledp! :ui theme doom-xcode)
    :doc "based off of Apple's Xcode Dark Theme (ported by @kadenbarlow)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-xcode t))

  (leaf doom-zenburn
    :disabled (gear-keep-disabledp! :ui theme doom-zenburn)
    :doc "port of the popular Zenburn theme (ported by @jsoa)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-zenburn t)))
