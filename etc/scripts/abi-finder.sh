#!/usr/bin/env bash
# abi-finder — Find the latest commit in a tree-sitter grammar repo whose
# compiled grammar matches a target ABI version.
#
# Usage: abi-finder <git-url> <target-abi> [source-dir] [lang-name]
#
#   git-url     — Git URL of the tree-sitter grammar repository
#   target-abi  — Maximum ABI version to accept (integer)
#   source-dir  — Subdirectory containing parser.c (default: src)
#   lang-name   — Language name for the tree_sitter_LANG symbol
#                 (default: inferred from the repo URL)

set -euo pipefail

# ── arguments ────────────────────────────────────────────────────────
url="${1:?usage: abi-finder <git-url> <target-abi> [source-dir] [lang-name]}"
target_abi="${2:?usage: abi-finder <git-url> <target-abi> [source-dir] [lang-name]}"
source_dir="${3:-src}"

# Infer language name from URL: strip trailing .git, take basename,
# remove the "tree-sitter-" prefix, replace hyphens with underscores.
if [[ -n "${4:-}" ]]; then
    lang="$4"
else
    basename="$(basename "$url" .git)"
    lang="${basename#tree-sitter-}"
    lang="${lang//-/_}"
fi

symbol="tree_sitter_${lang}"

# ── temp directory (cleaned up on exit) ──────────────────────────────
tmpdir="$(mktemp -d)"
trap 'rm -rf "$tmpdir"' EXIT

# ── ABI-check helper (compiled by Nix, path injected at build time) ──
abi_check="@ABI_CHECK@"

# ── clone the grammar repo ───────────────────────────────────────────
echo "Cloning $url ..." >&2
git clone --quiet "$url" "$tmpdir/repo"
cd "$tmpdir/repo"

# ── walk every commit, newest first ──────────────────────────────────
echo "Walking history for ABI <= $target_abi (symbol: $symbol) ..." >&2

found=0
commits="$(git log --format='%H')"

for commit in $commits; do
    # Check whether parser.c exists at this commit
    if ! git cat-file -e "$commit:$source_dir/parser.c" 2>/dev/null; then
        continue
    fi

    git checkout --quiet "$commit"

    # Collect source files
    sources=("$source_dir/parser.c")
    for extra in "$source_dir/scanner.c" "$source_dir/scanner.cc"; do
        [[ -f "$extra" ]] && sources+=("$extra")
    done

    # Pick compiler: use c++ if there is a .cc scanner, else cc
    compiler="cc"
    cc_flags=(-shared -fPIC -o "$tmpdir/grammar.so" -I "$source_dir")
    for src in "${sources[@]}"; do
        if [[ "$src" == *.cc ]]; then
            compiler="c++"
            break
        fi
    done

    # Compile (silence warnings; old commits may be noisy)
    if ! "$compiler" "${cc_flags[@]}" -w "${sources[@]}" 2>/dev/null; then
        echo "  $commit: compile failed, skipping" >&2
        continue
    fi

    # Read ABI from compiled .so
    abi="$("$abi_check" "$tmpdir/grammar.so" "$symbol" 2>/dev/null)" || {
        echo "  $commit: abi-check failed, skipping" >&2
        continue
    }

    echo "  $commit: ABI $abi" >&2

    if (( abi <= target_abi )); then
        found=1
        date="$(git log -1 --format='%aI' "$commit")"
        subject="$(git log -1 --format='%s' "$commit")"
        echo ""
        echo "commit:  $commit"
        echo "abi:     $abi"
        echo "date:    $date"
        echo "subject: $subject"
        break
    fi
done

if (( found == 0 )); then
    echo "" >&2
    echo "ERROR: No commit found with ABI <= $target_abi" >&2
    exit 1
fi
