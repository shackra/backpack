# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Summary

Backpack Emacs is a self-documenting GNU Emacs starter kit (requires Emacs 29.1+). Users declare features via a `gear!` DSL in `~/.backpack.d/init.el`. Uses **leaf.el** (not use-package) for configuration and **elpaca** for package management. Both are vendored as git submodules under `base-packages/`.

See `AGENTS.md` for full architecture documentation — it covers the three-tier taxonomy (pouches/gears/flags), boot sequence, gear file conventions, gating patterns, naming conventions, the inventory system, and benchmarks.

## Common Commands

### Development environment (Nix/devenv)

```sh
devenv shell              # Enter dev shell with all Emacs versions
devenv test               # Run tests across all Emacs versions (29.1 through rolling)
```

### Testing

```sh
# Single Emacs version
emacs --batch -l test/all-tests.el -f ert-run-tests-batch-and-exit

# Multi-version (requires devenv)
for-each-emacs $DEVENV_ROOT/.

# Test helper: copies config to tmpdir, runs tests
prepare-and-run
```

### Package sync and maintenance

```sh
bin/backpack ensure       # Batch-mode package install/build (run after config changes)
bin/backpack gc            # Remove orphaned packages
bin/backpack gc --dry-run  # Preview orphan cleanup
```

### Benchmarks

```sh
bin/backpack bench                                    # Full A/B benchmark suite
BACKPACK_BENCH_FILTER=subprocess bin/backpack bench   # Filter to specific benches
BACKPACK_BENCH_ITERATIONS=3 bin/backpack bench        # Fewer iterations for quick runs
```

### Inspecting Emacs state (with Anvil MCP)

Use Anvil MCP tools (`mcp__anvil__emacs-eval`) to inspect Emacs packages and runtime state instead of cloning repos to /tmp.

## Key Architecture Points

- **Gear files** live at `lisp/gears/<pouch>/<gear>.el` — they do NOT use `provide`; loaded directly via `load` calls in `backpack-load-gear-files`
- **Gear files are NOT auto-discovered for loading** — new gears must be manually added to `backpack-load-gear-files` in `lisp/backpack.el`
- **The inventory system** (`backpack-inventory.el`) discovers gears from the filesystem by parsing source files on demand — separate from the load mechanism
- **Core library** is in `lisp/` — `backpack.el` (orchestration), `backpack-pouch.el` (macro system), `backpack-sync.el` (elpaca integration), etc.
- **User config** is split-loaded: `gear!` form evaluated first, then all gear files load, then remaining user forms evaluate (so user customizations override gear defaults)

## Conventions

- All `.el` files use `lexical-binding: t`
- No `defcustom` — all config goes through `gear!`/`gearp!`
- All packages must use **pinned git refs**: `:ensure (pkg :ref "commit-hash")`
- `:ensure` is aliased to `:elpaca` via `leaf-alias-keyword-alist`
- Default-on features use negation gating: `(unless (gearp! :editing go -treesit) ...)`
- Opt-in features use positive gating: `(when (gearp! :editing go lsp) ...)`
- Hook functions use `-h` suffix (Doom convention): `backpack--reset-file-handler-alist-h`
- Private symbols use double-hyphen: `backpack--gear`
- User-facing utilities use slash: `backpack/mu4e-easy-context`
- Credit upstream projects (e.g., Doom Emacs) when adapting their code

## Adding a New Gear

1. Create `lisp/gears/<pouch>/<gear-name>.el`
2. Gate with `gearp!` in `:when`/`:unless`
3. Include `:doc` string on each leaf block
4. Use `:doctor` for external tool requirements, `:fonts` for font requirements
5. **Add a `load` call** in `backpack-load-gear-files` (`lisp/backpack.el`)
6. For tree-sitter support: use `backpack-treesit-langs!` gated behind `(not (gearp! :pouch gear -treesit))`

## CI

GitHub Actions runs `devenv test` which executes `for-each-emacs` against Emacs versions 29.1, 29.2, 29.3, 29.4, 30.1, and rolling.
