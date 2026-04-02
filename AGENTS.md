# AGENTS.md -- Backpack Emacs

## Project Overview

Backpack Emacs is a self-documenting GNU Emacs starter kit inspired by
[Bedrock](https://codeberg.org/ashton314/emacs-bedrock) and [Doom
Emacs](https://github.com/doomemacs/doomemacs). It requires **Emacs 29.1 or
newer**.

Users declare what features they want through a concise DSL (`gear!`) in their
private config file (`~/.backpack.d/init.el` or `$XDG_CONFIG_HOME/backpack/init.el`).
Backpack loads only what the user requests and auto-enables sensible defaults
that can be opted out of.

The configuration DSL is **leaf.el** (not `use-package`), and the package
manager is **elpaca**. Both are vendored as git submodules under
`base-packages/` so no network access is needed for bootstrap.

## Architecture

### Three-Tier Taxonomy

Backpack organises features into three levels:

| Concept  | Keyword style | Example              | Description                              |
|----------|---------------|----------------------|------------------------------------------|
| **Pouch** | `:keyword`   | `:editing`           | A category of related features           |
| **Gear**  | `symbol`     | `go`                 | A feature module (one `.el` file)        |
| **Flag**  | `symbol`     | `lsp`, `-treesit`    | An option that modifies a gear's behaviour|

Flags prefixed with `-` represent features that are **on by default** and can be
disabled by the user. Flags without `-` are **opt-in**.

Example user configuration:

```elisp
(gear!
  :ui
  (theme doom-one)
  :editing
  (go -treesit lsp)
  (python lsp)
  nix
  :config
  (default hide-menu-bar hide-tool-bar no-splash)
  :tools
  magit
  :checkers
  spellchecking)
```

### Core Library Files

All live under `lisp/`:

| File                       | Purpose                                                      |
|----------------------------|--------------------------------------------------------------|
| `backpack.el`              | Main module (~1000 lines): bootstrap, elpaca setup, startup optimisations, directory layout, gear loading, GC mode |
| `backpack-pouch.el`        | The `gear!`/`gearp!`/`gear-with-any-flagp!` macro system     |
| `backpack-email-utils.el`  | `backpack/mu4e-easy-context` helper macro                    |
| `backpack-inventory.el`    | Self-documenting inventory browser (`M-x backpack-inventory`)|

### Gear Files

Each gear is a standalone `.el` file under `lisp/gears/<pouch>/<gear>.el`.
Gear files do **not** use `provide` -- they are loaded directly via `load` calls
in `backpack-load-gear-files`. Every gear file self-gates using `gearp!` checks
so it is safe to load all of them unconditionally.

## Directory Structure

```
emacs-backpack/
├── early-init.el                  # Emacs entry point; loads backpack.el, calls backpack-start
├── ensure.el                      # Batch-mode package sync (backpack ensure)
├── gc.el                          # Batch-mode orphan package cleanup (backpack gc)
│
├── lisp/                          # Core Backpack Emacs Lisp library
│   ├── backpack.el                # Main module
│   ├── backpack-pouch.el          # gear!/gearp! configuration query system
│   ├── backpack-email-utils.el    # mu4e context helper
│   ├── backpack-inventory.el      # Self-documenting inventory browser
│   └── gears/                     # All feature modules, organised by pouch
│       ├── config/
│       │   └── default.el
│       ├── ui/
│       │   ├── theme.el
│       │   └── treesit.el
│       ├── completion/
│       │   ├── corfu.el
│       │   ├── eglot.el
│       │   ├── marginalia.el
│       │   ├── nerd-icons-completion.el
│       │   ├── orderless.el
│       │   └── vertico.el
│       ├── tools/
│       │   ├── cool-motions.el
│       │   ├── eldoc.el
│       │   ├── envrc.el
│       │   ├── magit.el
│       │   └── whitespaces.el
│       ├── checkers/
│       │   └── spellchecking.el
│       ├── email/
│       │   └── mu4e.el
│       └── editing/
│           ├── emacs-lisp.el
│           ├── go.el
│           ├── haskell.el
│           ├── hyprland.el
│           ├── json.el
│           ├── latex.el
│           ├── lua.el
│           ├── markdown.el
│           ├── nix.el
│           ├── org.el
│           ├── python.el
│           ├── rst.el
│           ├── rust.el
│           ├── terraform.el
│           ├── toml.el
│           └── yaml.el
│
├── base-packages/                 # Vendored dependencies (git submodules)
│   ├── elpaca/                    # Package manager
│   ├── leaf.el/                   # Configuration DSL
│   └── leaf-keywords.el/          # Extended leaf keywords
│
├── bin/
│   └── backpack                   # Shell CLI (backpack ensure, backpack gc)
│
├── test/
│   ├── all-tests.el               # Test runner
│   ├── startup-time.el            # Startup time benchmark
│   └── pouch/
│       └── backpack-pouch.el      # Unit tests for gear!/gearp!
│
├── etc/scripts/
│   ├── prepare-and-run.sh         # Test helper: copy config to tmpdir, run tests
│   └── for-each-emacs.sh          # Run tests against multiple Emacs versions
│
├── .cache/                        # Runtime cache (gitignored)
│   ├── etc/                       # Important data files
│   └── nonessentials/             # Deletable cache (elpaca builds, tree-sitter grammars)
│
├── devenv.nix                     # Nix dev environment (Emacs 29.1 through rolling)
├── devenv.yaml                    # Nix inputs for multiple Emacs versions
├── .github/workflows/ci.yml       # CI: Nix + devenv across Emacs versions
├── .dir-locals.el                 # Per-project Emacs settings
├── .envrc                         # direnv integration
└── .gitmodules                    # Submodule declarations
```

## Boot Sequence

### Normal interactive startup

```
early-init.el
 └─ load lisp/backpack.el
     ├─ Checks Emacs >= 29.1
     ├─ Adds base-packages/ to load-path
     ├─ (require 'leaf), (require 'leaf-keywords)
     ├─ (require 'backpack-pouch), (require 'backpack-email-utils), (require 'backpack-inventory)
     ├─ Sets up elpaca from base-packages/ (offline, no internet)
     └─ Defines backpack-start, backpack-finalize, backpack-load-gear-files
 └─ (backpack-start t)
     ├─ Creates required directories (.cache/etc, .cache/nonessentials, etc.)
     ├─ Loads user init: $backpack-user-dir/init.el  (contains the gear! declaration)
     ├─ Loads custom.el
     ├─ Calls backpack-load-gear-files
     │   └─ Loads ALL gear files in explicit order (each self-gates with gearp!)
     ├─ Adds backpack-finalize as advice on command-line-1
     └─ On finalize: runs backpack-after-init-hook → activates packages via elpaca
```

### Batch sync mode (`backpack ensure`)

```
bin/backpack ensure
 → emacs --batch -l ensure.el
    ├─ Sets backpack-mode to 'sync
    ├─ Loads backpack.el, user init.el
    ├─ Calls backpack-load-gear-files (queues packages for install/build)
    ├─ Waits for elpaca to install/build all packages
    ├─ Activates enable-on-sync packages (e.g., treesit-auto)
    ├─ Installs tree-sitter grammars
    └─ Exits
```

### Garbage collection (`backpack gc`)

```
bin/backpack gc [--dry-run]
 → emacs --batch -l gc.el
    ├─ Collects declared packages from gear files
    ├─ Compares against installed packages
    └─ Deletes orphaned packages (or reports in dry-run)
```

## Gear File Conventions

### Structure of a gear file

A typical gear file looks like this:

```elisp
;; Declare tree-sitter languages (if applicable)
(when (and (gearp! :editing go)
           (not (gearp! :editing go -treesit)))
  (backpack-treesit-langs! go gomod)
  (add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode)))

;; Main leaf block
(leaf go-mode
  :doc "Support for Go programming language in Emacs"
  :when (gearp! :editing go)
  :ensure (go-mode :ref "0ed3c5227e7f622589f1411b4939c3ee34711ebd")
  :hook
  ((go-mode-hook go-ts-mode-hook) . electric-pair-local-mode)
  :config
  ;; Nested leaf for an opt-in sub-feature
  (leaf eglot
    :doc "Language Server Protocol support for go-mode"
    :when (gearp! :editing go lsp)
    :doctor ("gopls" . "the official LSP implementation provided by the Go team")
    :hook ((go-mode-hook go-ts-mode-hook) . eglot-ensure)))
```

### Gating Patterns

There are four patterns for controlling when a gear or sub-feature loads:

**Pattern A -- Opt-in gear** (user must list it):

```elisp
(leaf magit
  :when (gearp! :tools magit)
  ...)
```

**Pattern B -- Default-on gear** (user must negate to disable):

```elisp
(leaf ws-butler
  :unless (gearp! :tools -whitespaces)
  ...)
```

**Pattern C -- Opt-in flag** within a gear:

```elisp
(leaf eglot
  :when (gearp! :editing go lsp)
  ...)
```

**Pattern D -- Default-on flag** (on unless user negates):

```elisp
(unless (gearp! :editing go -display-line-numbers)
  (display-line-numbers-mode +1))
```

### Custom leaf Keywords

Backpack extends leaf.el with two custom keywords:

- **`:doctor`** -- Declares external binaries required by a feature. Takes cons
  pairs of `("binary-name" . "description")`.
- **`:fonts`** -- Declares fonts required by a feature. Same format as `:doctor`.

These keywords are metadata-only during normal load. They are parsed by
`backpack-inventory.el` for the self-documenting help system.

### Package Pinning

All packages use pinned git refs via `:ensure`:

```elisp
:ensure (go-mode :ref "0ed3c5227e7f622589f1411b4939c3ee34711ebd")
```

The `:ensure` keyword is aliased to `:elpaca` internally.

## Naming Conventions

| Pattern           | Example                                    | Usage                                    |
|-------------------|--------------------------------------------|------------------------------------------|
| `backpack-*`      | `backpack-emacs-dir`, `backpack-start`     | Public API / variables                   |
| `backpack--*`     | `backpack--gear`, `backpack--treesit-langs`| Internal/private symbols (double hyphen) |
| `backpack/*`      | `backpack/mu4e-easy-context`               | User-facing utility functions            |
| `gear!`           | `gear!`                                    | Declarative macro (bang = side-effectful)|
| `gearp!`          | `gearp!`                                   | Predicate macro (p = predicate)          |
| `*-p`             | `backpack-sync-mode-p`                     | Boolean predicate functions              |
| `backpack--*-h`   | `backpack--reset-file-handler-alist-h`     | Hook functions (h suffix, Doom convention)|
| `-flag`           | `-treesit`, `-display-line-numbers`        | Negation/opt-out flags in gear!          |

Gear files do **not** use a `backpack-gear-` prefix. Leaf blocks are named after
the package they configure (e.g., `go-mode`, `corfu`, `jinx`).

## The `backpack-inventory` System

`M-x backpack-inventory` opens a hierarchical, navigable help buffer with three
levels:

1. **Pouch listing** -- all pouches with descriptions and gear counts
2. **Gear listing** -- all gears in a pouch with enabled/disabled/default-on status
3. **Gear detail** -- full info: description, flags, external tools, fonts, example `gear!` snippet

Navigation:

- `RET` -- drill into a pouch or gear
- `l` or `DEL` -- go back (browser-like history stack)
- `g` -- refresh (re-scan files)
- `q` -- quit

### How It Works

The system **parses gear source files on demand** using Emacs's `read` function.
It does **not** require annotations or metadata files. It discovers pouches by
scanning subdirectories of `lisp/gears/`, and gears by listing `.el` files in
each pouch directory.

For each file, it reads S-expressions and recursively walks them to extract:

- `gearp!` / `gear-with-any-flagp!` calls (determines gear names, flags, and
  default-on status)
- `:doc` strings from leaf blocks
- `:doctor` entries (external tool requirements)
- `:fonts` entries (font requirements)
- `:when` / `:unless` context (determines whether a feature is opt-in or
  default-on)

It handles all the edge cases in the codebase: `(not (gearp! ...))` context
flipping, `(or ...)` / `(and ...)` compound conditions, cross-gear references,
gear names that differ from filenames (e.g., `envrc.el` defines the `direnv`
gear), and the `gear-with-any-flagp!` macro used in the theme system.

Parsing adds zero startup cost since it only runs when the user invokes the
command.

## Testing

### Running Tests

Tests use ERT (Emacs Regression Testing). The test suite lives in `test/`.

With Nix/devenv available:

```sh
devenv test                # Runs tests across all Emacs versions (29.1 through rolling)
```

Manually:

```sh
emacs --batch -l test/all-tests.el -f ert-run-tests-batch-and-exit
```

### Multi-Version Testing

The `devenv.nix` environment provides Emacs versions 29.1, 29.2, 29.3, 29.4,
30.1, and rolling. CI runs `devenv test` which executes
`etc/scripts/for-each-emacs.sh` to test against all versions.

## Key Conventions and Rules

### General

- All Emacs Lisp files use `lexical-binding: t`.
- There are **no `defcustom` variables** in the project. Configuration is
  entirely through the `gear!` / `gearp!` macro system.
- The user's `gear!` declaration is the single source of truth for what features
  are active. It is stored in `backpack--gear` at runtime.

### Adding a New Gear

To add a new gear:

1. Create `lisp/gears/<pouch>/<gear-name>.el`.
2. Use `gearp!` in `:when` / `:unless` to gate the gear's leaf blocks.
3. Include a `:doc` string on each leaf block for the inventory system.
4. If external tools are needed, use the `:doctor` keyword.
5. If fonts are needed, use the `:fonts` keyword.
6. **Add a `load` call** to `backpack-load-gear-files` in `lisp/backpack.el`.
   Gear files are **not** auto-discovered for loading -- they must be explicitly
   listed. (The inventory system discovers them from the filesystem, but the
   load order is explicit.)
7. If the gear supports tree-sitter, use `backpack-treesit-langs!` to declare
   the needed grammars, gated behind `(not (gearp! :pouch gear -treesit))`.

### Adding a New Pouch

To add a new pouch:

1. Create a directory `lisp/gears/<pouch-name>/`.
2. Add gear files inside it.
3. Add a description to `backpack-inventory--pouch-descriptions` in
   `lisp/backpack-inventory.el`.
4. Optionally add it to `backpack-inventory--pouch-order` if you want it to
   appear in a specific position in the inventory browser.

### Package References

All external packages must use pinned git refs. Never use `:ensure t` or
unpinned refs. This ensures reproducible builds.

### The `-flag` Convention

When a sub-feature should be **on by default** (like tree-sitter support or line
numbers), gate it with a negation check:

```elisp
(unless (gearp! :editing go -display-line-numbers)
  (display-line-numbers-mode +1))
```

This means the feature is active unless the user explicitly adds `-display-line-numbers`
to their gear declaration. For top-level gear defaults, use `:unless (gearp! :pouch -gearname)`.

### Cross-Gear References

Some gears reference other gears (e.g., `haskell.el` checking `(gearp! :editing org)`
for org-babel integration). These are dependency checks, not gear definitions.
The inventory parser handles these by preferring the gear entry from the file
whose name matches the gear.
