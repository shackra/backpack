set -euo pipefail

# run-e2e-treesit.sh -- End-to-end tree-sitter test orchestrator.
#
# Copies the repo to a temp directory, generates an init.el from the
# test declarations in test/e2e-treesit.el, runs `backpack ensure' to
# install packages and tree-sitter grammars, then runs the ERT tests
# that verify tree-sitter activates correctly for each gear.
#
# Usage: run-e2e-treesit <emacs-binary> <source-dir>
#
# Example:
#   run-e2e-treesit emacs .
#   run-e2e-treesit emacs-rolling /path/to/emacs-backpack

# -- 1. Prepare: validate args, copy repo to tmpdir ---------------------

TEST_HOME=$(prepare "$1" "$2")
trap "rm -rf $TEST_HOME" EXIT

EMACS="$1"
EMACS_D="$TEST_HOME/.emacs.d"
BACKPACK_D="$TEST_HOME/.backpack.d"

mkdir -p "$BACKPACK_D"

# -- 2. Generate init.el from test declarations -------------------------
#    Load e2e-treesit.el in a bare Emacs (only needs ert) and call
#    backpack-e2e--print-init-el to emit the gear! form to stdout.

echo "Generating init.el from test declarations..."

$EMACS --batch \
    -l ert \
    -l "$EMACS_D/test/e2e-treesit.el" \
    --eval "(backpack-e2e--print-init-el)" \
    > "$BACKPACK_D/init.el"

echo "Generated init.el:"
cat "$BACKPACK_D/init.el"
echo ""

# -- 3. Run backpack ensure --------------------------------------------
#    Install packages and tree-sitter grammars.  backpack-user-dir is
#    set via --eval BEFORE loading ensure.el so the defvar in
#    backpack.el does not overwrite it.

echo "Running backpack ensure..."

$EMACS --batch \
    --eval "(setq user-emacs-directory \"$EMACS_D/\")" \
    --eval "(setq backpack-user-dir \"$BACKPACK_D/\")" \
    -l "$EMACS_D/ensure.el"

echo ""
echo "backpack ensure completed."
echo ""

# -- 4. Run E2E ERT tests ----------------------------------------------
#    Boot a fresh Emacs through the full Backpack startup sequence
#    (early-init.el -> backpack.el -> backpack-start) with our custom
#    backpack-user-dir, then load and execute the tests.

echo "Running E2E tree-sitter tests..."

$EMACS --init-directory "$EMACS_D" -batch \
    --eval "(setq backpack-user-dir \"$BACKPACK_D/\")" \
    -l "$EMACS_D/early-init.el" \
    -l ert \
    -l "$EMACS_D/test/e2e-treesit.el" \
    -f ert-run-tests-batch-and-exit
