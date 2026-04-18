#!/usr/bin/env bash

set -euo pipefail

# drive-conf.sh -- Prepare Backpack, run ensure, open Emacs interactively.
#
# Usage: drive-conf <emacs-binary> <user-config-dir> <project-dir>
#
# The user-config-dir MUST be an existing directory with an init.el.
# The project-dir MUST be the root of the Backpack repository.
# Copies project to tmpdir, runs backpack ensure, then launches Emacs.

if [ $# -ne 3 ]; then
    echo "Usage: $0 <emacs-binary> <user-config-dir> <project-dir>" >&2
    echo "" >&2
    echo "Example: $0 emacs-29-4 /tmp/my-config /path/to/backpack" >&2
    echo "" >&2
    echo "The user-config-dir must exist and contain an init.el." >&2
    echo "The project-dir must be the Backpack repository root." >&2
    exit 1
fi

EMACS_BIN="$1"
CONF_DIR="$2"
BACKPACK_DIR="$3"

if ! command -v "$EMACS_BIN" >/dev/null 2>&1; then
    echo "Error: $EMACS_BIN not found" >&2
    exit 1
fi

if [ ! -d "$CONF_DIR" ]; then
    echo "Error: $CONF_DIR is not a directory" >&2
    exit 1
fi

if [ ! -f "$CONF_DIR/init.el" ]; then
    echo "Error: $CONF_DIR/init.el not found" >&2
    echo "Create an init.el with your gear! declaration first." >&2
    exit 1
fi

if [ ! -f "$BACKPACK_DIR/ensure.el" ] || [ ! -d "$BACKPACK_DIR/base-packages" ]; then
    echo "Error: $BACKPACK_DIR is not a Backpack project root" >&2
    exit 1
fi

# Copy project to tmpdir (exclude .git, .github, .cache)
TEST_HOME=$(mktemp -d)
trap 'rm -rf "$TEST_HOME"' EXIT

rsync -a \
    --filter=':- .gitignore' \
    --filter='- .git/' \
    --filter='- .github/' \
    --filter='- .cache/' \
    "$BACKPACK_DIR/" "$TEST_HOME/.emacs.d"

# Show info
$EMACS_BIN --version | head -n 1 >&2
echo "Backpack dir: $TEST_HOME/.emacs.d" >&2
echo "Config dir:   $CONF_DIR" >&2
echo "" >&2

# Run backpack ensure
echo "Running backpack ensure..." >&2
BACKPACK_USER_DIR="$CONF_DIR" \
    $EMACS_BIN --batch \
        --eval "(setq user-emacs-directory \"$TEST_HOME/.emacs.d/\")" \
        --eval "(setq backpack-user-dir \"$CONF_DIR\")" \
        -l "$TEST_HOME/.emacs.d/ensure.el"

ENSURE_EXIT=$?

if [ $ENSURE_EXIT -ne 0 ]; then
    echo "FAILED: backpack ensure exited with code $ENSURE_EXIT" >&2
    exit $ENSURE_EXIT
fi

echo "" >&2
echo "========================================" >&2
echo "Opening Emacs interactively..." >&2
echo "Press C-x C-c to exit." >&2
echo "========================================" >&2

# Open Emacs interactively
BACKPACK_USER_DIR="$CONF_DIR" \
    $EMACS_BIN --init-directory "$TEST_HOME/.emacs.d"