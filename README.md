# Backpack Emacs

![Only compatible with GNU Emacs 29.1 and up](https://img.shields.io/badge/compatible-29.1%20and%20up-grey?style=for-the-badge&logo=gnuemacs&logoColor=white&logoSize=auto&label=GNU%20Emacs&labelColor=%237F5AB6)

A self-documenting GNU Emacs starter kit inspired by [Bedrock](https://codeberg.org/ashton314/emacs-bedrock) and [Doom](https://github.com/doomemacs/doomemacs).

> [!WARNING]  
> This starter kit is bare bones and in rapid development, things may not work, be missing or break in following releases.

This starter kit uses [leaf.el](https://github.com/conao3/leaf.el) as the DSL for the configuration and [elpaca](https://github.com/progfolio/elpaca) as package manager. There are macros and functions that help users defining what they want to use from the starter kit (much like in Doom; except I took some liberty to auto-turn on some features).

## Getting started

### Installing Backpack Emacs

Make a backup of your old Emacs configuration. Then, clone this repository either in your `$HOME` or in your `$XDG_CONFIG_HOME`:

```sh
git clone --recurse-submodules https://github.com/shackra/backpack.git ~/.emacs.d
```

Be sure to put `$HOME/.emacs.d/bin/` in your path, otherwise your shell won't know where `backpack` is.

On Windows, use `backpack.cmd` instead of `backpack`:

```
backpack.cmd ensure
backpack.cmd gc --dry-run
```

Make sure `emacs.exe` is on your PATH.

### Writing your initial configuration

Now that you have the starter kit cloned with its corresponding sub-modules (right?), you need to write your configuration. It has to go in either `$HOME/.backpack.d` or `$XDG_CONFIG_HOME/backpack`, in any of those two directories make a file called `init.el`:

```elisp
;; -*- no-byte-compile: t; -*-

(gear!
  :ui
  (theme modus-vivendi)
  :editing
  (go -treesit lsp)
  :config
  default)
```

This will tell Backpack Emacs what stuff you want to use. There are *pouches* (or categories, `:ui` for example) that store different *gear* (or modules, `theme` is gear). Gear can have extra features you may want to turn on (or off), Backpack Emacs expresses the existence of such additional features as *flags* (the *flag* of `theme` is `modus-vivendi`, the theme we want to use in the example above).

Some features are turned on by default if some gear is activated. To turn them off, simply put a dash in front of the flag (`-treesit` in the go gear, for example). Putting a dash in front of a flag that is activated only when explicitly used by the user does nothing.

### Please, run `backpack ensure` on a terminal every time you change your configuration

Please do the above. It helps speed up the initialization of Emacs and run functions that won't be called in a normal execution of Emacs. When running `backpack ensure` you are supposed to see the elpaca information panel, among many other things, and then Emacs exiting.

### Finding out what is available in your Backpack

Press `C-h B` or run `M-x backpack-inventory` to browse all available pouches, gears and flags interactively. The inventory shows what's enabled in your configuration, what's available, and how to configure each feature -- including flags, external tools, required fonts, and pinned package versions.

## What's in the Backpack?

Backpack is self-documenting. Rather than maintaining a static list of features here, run `M-x backpack-inventory` (or `C-h B`) inside Emacs to see everything that's available. The inventory browser lets you navigate pouches, drill into gears, and inspect flags, external tool requirements, fonts, example `gear!` snippets, and pinned package refs -- all extracted automatically from the source code.

## Tips

### Email contexts with mu4e

If you enable the `:email mu4e` gear, you can use `backpack/mu4e-easy-context` to easily create your email account contexts in your configuration:

```elisp
(with-eval-after-load 'mu4e
  (setq mu4e-contexts `(,(backpack/mu4e-easy-context
                          :c-name "example"
                          :maildir "example-dir"
                          :mail "user@example.com"
                          :sig "Example signature"))))
```
