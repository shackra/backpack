(leaf theme
  :doc "Emacs theme configuration"
  :tag "ui" "theme"
  :config ;; TODO(shackra): Indicate in theme that only one flag/feature can be chosen
  (leaf doom-one
    :disabled t
    :doc "inspired by Atom One Dark (ported by @hlissner)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-one t))
  
  (leaf doom-1337
    :disabled t
    :doc "ported from VSCode's 1337 theme (ported by @ccmywish)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-1337 t))
  
  (leaf doom-acario-dark
    :disabled t
    :doc "an original dark theme (ported by @gagbo)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-acario-dark t))
  
  (leaf doom-acario-light
    :disabled t
    :doc "an original light theme (ported by @gagbo)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-acario-light t))
  
  (leaf doom-ayu-dark
    :disabled t
    :doc "inspired by Ayu Dark (ported by @ashton)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-ayu-dark t))
  
  (leaf doom-ayu-light
    :disabled t
    :doc "inspirted by Ayu Light (ported by @LoveSponge)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-ayu-light t))
  
  (leaf doom-ayu-mirage
    :disabled t
    :doc "inspired by Ayu Mirage (ported by @LoveSponge)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-ayu-mirage t))
  
  (leaf doom-badger 
    :disabled t
    :doc "inspired by cann's Badger colorscheme (ported by @jsoa)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-badger t))
  
  (leaf doom-challenger-deep 
    :disabled t
    :doc "inspired by Vim's Challenger Deep theme (ported by @fuxialexander)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-challenger-deep t))
  
  (leaf doom-city-lights 
    :disabled t
    :doc "inspired by Atom's City Lights theme (ported by @fuxialexander)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-city-lights t))
  
  (leaf doom-dark+ 
    :disabled t
    :doc "ported from equinusocio's VSCode Theme, dark+ (ported by @ema2159)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-dark+ t))
  
  (leaf doom-dracula 
    :disabled t
    :doc "inspired by the popular Dracula theme (ported by @fuxialexander)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-dracula t))
  
  (leaf doom-earl-grey 
    :disabled t
    :doc "a gentle color scheme, for code (ported by @JuneKelly)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-earl-grey t))
  
  (leaf doom-ephemeral 
    :disabled t
    :doc "inspired by the Ephemeral Theme from elenapan's dotfiles (ported by @karetsu)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-ephemeral t))
  
  (leaf doom-fairy-floss 
    :disabled t
    :doc "a candy colored theme by sailorhg (ported by @ema2159)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-fairy-floss t))
  
  (leaf doom-feather-dark 
    :disabled t
    :doc "a purple-tinted theme, inspired by doom-one (by @Plunne)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-feather-dark t))
  
  (leaf doom-feather-light 
    :disabled t
    :doc "a light variable of feather-dark, inspired by doom-one (by @Plunne)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-feather-light t))
  
  (leaf doom-flatwhite
    :disabled t
    :doc "inspired by Atom's Flatwhite Syntax theme (ported by @JuneKelly)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-flatwhite t))
  
  (leaf doom-gruvbox 
    :disabled t
    :doc "inspired by morhetz's Gruvbox (ported by @JongW)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-gruvbox t))
  
  (leaf doom-gruvbox-light 
    :disabled t
    :doc "inspired by morhetz's Gruvbox (light) (ported by @jsoa)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-gruvbox-light t))
  
  (leaf doom-henna
    :disabled t
    :doc "based on VSCode's Henna theme (ported by @jsoa)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-henna t))
  
  (leaf doom-homage-black 
    :disabled t
    :doc "a minimalistic, colorless theme inspired by eziam, tao, and jbeans (ported by @mskorzhinskiy)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-homage-black t))
  
  (leaf doom-homage-white 
    :disabled t
    :doc "minimal white theme inspired by editors from 2000s (ported by @mskorzhinskiy)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-homage-white t))
  
  (leaf doom-horizon 
    :disabled t
    :doc "ported from VSCode Horizon (ported by @karetsu)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-horizon t))
  
  (leaf doom-Iosvkem 
    :disabled t
    :doc "ported from the default dark theme for Adobe Brackets (ported by @neutaaaaan)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-Iosvkem t))
  
  (leaf doom-ir-black 
    :disabled t
    :doc "ported from Vim's ir_black colorscheme (ported by @legendre6891)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-ir-black t))
  
  (leaf doom-lantern 
    :disabled t
    :doc "based on Gitleptune's Lantern theme (ported by @paladhammika)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-lantern t))
  
  (leaf doom-laserwave 
    :disabled t
    :doc "a clean synthwave/outrun theme inspired by VSCode's Laserwave (ported by @hyakt)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-laserwave t))
  
  (leaf doom-manegarm 
    :disabled t
    :doc "an original autumn-inspired dark theme (ported by @kenranunderscore)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-managarm t))
  
  (leaf doom-material 
    :disabled t
    :doc "adapted from equinusocio's Material themes (ported by @tam5)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-material t))
  
  (leaf doom-material-dark 
    :disabled t
    :doc "inspired by Material Theme by xrei (ported by @trev-dev)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-material-dark t))
  
  (leaf doom-meltbus 
    :disabled t
    :doc "a dark (mostly) monochromatic theme (ported by @spacefrogg)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-meltbus t))
  
  (leaf doom-miramare 
    :disabled t
    :doc "a port of Franbach's Miramare theme; a variant of Grubox (ported by @sagittaros)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-miramare t))
  
  (leaf doom-molokai 
    :disabled t
    :doc "inspired by Tomas Restrepo's Molokai (ported by @hlissner)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-molokai t))
  
  (leaf doom-monokai-classic 
    :disabled t
    :doc "port of Monokai Classic (ported by @ema2159)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-monokai-classic t))
  
  (leaf doom-monokai-machine 
    :disabled t
    :doc "port of Monokai Classic (ported by @minikN)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-monokai-machine t))
  
  (leaf doom-monokai-octagon 
    :disabled t
    :doc "port of Monokai Octagon (ported by @minikN)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-monokai-octagon t))
  
  (leaf doom-monokai-pro 
    :disabled t
    :doc "Port of Monokai Pro (ported by @minikN)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-monokai-pro t))
  
  (leaf doom-monokai-ristretto 
    :disabled t
    :doc "Port of Monokai Ristretto (ported by @minikN)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-monokai-ristretto t))
  
  (leaf doom-monokai-spectrum 
    :disabled t
    :doc "port of Monokai Spectrum (ported by @minikN)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-monokai-spectrum t))
  
  (leaf doom-moonlight 
    :disabled t
    :doc "inspired by VS code's Moonlight (ported by @Brettm12345)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-moonlight t))
  
  (leaf doom-nord 
    :disabled t
    :doc "dark variant of Nord (ported by @fuxialexander)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-nord t))
  
  (leaf doom-nord-aurora 
    :disabled t
    :doc "a light variant of Nord (ported by @MoskitoHero)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-nord-aurora t))
  
  (leaf doom-nord-light 
    :disabled t
    :doc "light variant of Nord (ported by @fuxialexander)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-nord-light t))
  
  (leaf doom-nova 
    :disabled t
    :doc "inspired by Trevord Miller's Nova (ported by @bigardone)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-nova t))
  
  (leaf doom-oceanic-next 
    :disabled t
    :doc "inspired by Oceanic Next (ported by @juanwolf)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-oceanic-next t))
  
  (leaf doom-oksolar-dark 
    :disabled t
    :doc "an OKLab variant of Solarized dark (ported by @logc)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-oksolar-dark t))
  
  (leaf doom-oksolar-light 
    :disabled t
    :doc "an OKLab variant of Solarized light (ported by @logc)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-oksolar-light t))
  
  (leaf doom-old-hope 
    :disabled t
    :doc "inspired by An Old Hope, in a galaxy far far away (ported by @teesloane)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-old-hope t))
  
  (leaf doom-one-light 
    :disabled t
    :doc "inspired by Atom One Light (ported by @ztlevi)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-one-light t))
  
  (leaf doom-opera 
    :disabled t
    :doc "an original light theme (ported by @jwintz)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-opera t))
  
  (leaf doom-opera-light 
    :disabled t
    :doc "an original light theme (ported by @jwintz)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-opera-light t))
  
  (leaf doom-outrun-electric 
    :disabled t
    :doc "a high contrast, neon theme inspired by Outrun Electric on VSCode (ported by @ema2159)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-outrun-electric t))
  
  (leaf doom-palenight 
    :disabled t
    :doc "adapted from equinusocio's Material themes (ported by @Brettm12345)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-palenight t))
  
  (leaf doom-peacock 
    :disabled t
    :doc "inspired by daylerees' Peacock (ported by @teesloane)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-peacock t))
  
  (leaf doom-pine 
    :disabled t
    :doc "a green flavor of doom-gruvbox (by @RomanHargrave)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-pine t))
  
  (leaf doom-plain 
    :disabled t
    :doc "inspired by gko's plain theme for VSCode (ported by @das-s)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-plain t))
  
  (leaf doom-plain-dark 
    :disabled t
    :doc "inspired by gko's plain theme for VSCode (ported by @das-s)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-plain-dark t))
  
  (leaf doom-rouge 
    :disabled t
    :doc "ported from VSCode's Rouge Theme (ported by @das-s)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-rouge t))
  
  (leaf doom-shades-of-purple 
    :disabled t
    :doc "a port of VSCode's Shades of Purple (ported by @jwbaldwin)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-shades-of-purple t))
  
  (leaf doom-snazzy 
    :disabled t
    :doc "inspired by Hyper Snazzy (ported by @ar1a)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-snazzy t))
  
  (leaf doom-solarized-dark 
    :disabled t
    :doc "a dark variant of Solarized (ported by @ema2159)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-solarized-dark t))
  
  (leaf doom-solarized-dark-high-contrast 
    :disabled t
    :doc "a high-contrast variant of Solarized Dark (ported by @jmorag)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-solarized-dark-high-contrast t))
  
  (leaf doom-solarized-light 
    :disabled t
    :doc "a light variant of Solarized (ported by @fuxialexander)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-solarized-light t))
  
  (leaf doom-sourcerer 
    :disabled t
    :doc "a port of xero's Sourcerer (ported by @fm0xb)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-sourcerer t))
  
  (leaf doom-spacegrey 
    :disabled t
    :doc "I'm sure you've heard of it (ported by @teesloane)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-spacegrey t))
  
  (leaf doom-tokyo-night 
    :disabled t
    :doc "inspired by VSCode's Tokyo Night theme (ported by @FosterHangdaan)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-tokyo-night t))
  
  (leaf doom-tomorrow-day 
    :disabled t
    :doc "a light variant of Tomorrow (ported by @emacswatcher)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-tomorrow-day t))
  
  (leaf doom-tomorrow-night 
    :disabled t
    :doc "One of the dark variants of Tomorrow (ported by @hlissner)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-tomorrow-night t))
  
  (leaf doom-vibrant 
    :disabled t
    :doc "a more vibrant variant of doom-one (ported by @hlissner)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-vibrant t))
  
  (leaf doom-wilmersdorf 
    :disabled t
    :doc "port of Ian Pan's Wilmersdorf (ported by @ema2159)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-wilmersdorf t))
  
  (leaf doom-xcode 
    :disabled t
    :doc "based off of Apple's Xcode Dark Theme (ported by @kadenbarlow)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-xcode t))
  
  (leaf doom-zenburn 
    :disabled t
    :doc "port of the popular Zenburn theme (ported by @jsoa)"
    :ensure (doom-themes :ref "556598955c67540eac8811835b327f299ffb58c7")
    :custom
    (doom-themes-enable-bold . t)
    (doom-themes-enable-italic . t)
    :config
    (load-theme 'doom-zenburn t)))
