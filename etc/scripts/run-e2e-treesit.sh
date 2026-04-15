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
#    Load e2e-treesit.el in a bare Emacs (only needs ert) with the
#    generate-only guard set so ert-deftest forms are skipped -- we
#    only need the accumulated gear specs to emit the gear! form.

echo "Generating init.el from test declarations..."

if ! $EMACS --batch \
    -l ert \
    --eval "(setq backpack-e2e--generate-only t)" \
    -l "$EMACS_D/test/e2e-treesit.el" \
    --eval "(backpack-e2e--print-init-el)" \
    > "$BACKPACK_D/init.el" 2>"$TEST_HOME/step2.log"; then
    echo "FAILED: init.el generation." >&2
    cat "$TEST_HOME/step2.log" >&2
    exit 1
fi

echo "Generated init.el:"
cat "$BACKPACK_D/init.el"
echo ""

# -- 3. Run backpack ensure --------------------------------------------
#    Install packages and tree-sitter grammars.  backpack-user-dir is
#    set via --eval BEFORE loading ensure.el so the defvar in
#    backpack.el does not overwrite it.

echo "Running backpack ensure..."

if ! $EMACS --batch \
    --eval "(setq user-emacs-directory \"$EMACS_D/\")" \
    --eval "(setq backpack-user-dir \"$BACKPACK_D/\")" \
    -l "$EMACS_D/ensure.el" 2>&1; then
    echo ""
    echo "FAILED: backpack ensure." >&2
    exit 1
fi

echo ""
echo "backpack ensure completed."
echo ""

# -- 4. Run E2E tests in an interactive Emacs session -------------------
#    We launch Emacs interactively (not --batch) so that elpaca's
#    asynchronous package activation runs normally through the event
#    loop.  The test file hooks backpack-e2e--run-tests into
#    elpaca-after-init-hook (via -f backpack-e2e--run-and-exit) so it
#    runs only after every package is fully activated.  Results are
#    written to a file; the exit code equals the number of failures.
#
#    `script -qec` provides a pseudo-tty so Emacs doesn't complain
#    about "standard input is not a tty".
#
#    BACKPACK_USER_DIR is exported as an environment variable so that
#    backpack.el reads it at defvar time (before backpack-start loads
#    the user init).  Using --eval to set backpack-user-dir is too late
#    in interactive mode: --eval args are processed during command-line-1
#    which runs after early-init and the user init file have already been
#    loaded.

echo "Running E2E tree-sitter tests..."

RESULTS_FILE="$TEST_HOME/e2e-results.txt"

set +e
BACKPACK_USER_DIR="$BACKPACK_D/" \
script -qec "$EMACS --init-directory $EMACS_D \
    --eval \"(setq backpack-e2e--results-file \\\"$RESULTS_FILE\\\")\" \
    --eval \"(setq backpack-e2e--generate-only t)\" \
    -l $EMACS_D/test/e2e-treesit.el \
    -f backpack-e2e--run-and-exit" /dev/null > /dev/null 2>&1
TEST_EXIT=$?
set -e

echo ""
if [ -f "$RESULTS_FILE" ]; then
    cat "$RESULTS_FILE"
else
    echo "FAILED: no results file produced.  Emacs may have crashed." >&2
    echo "Re-running without output suppression for diagnostics..." >&2
    BACKPACK_USER_DIR="$BACKPACK_D/" \
    script -qec "$EMACS --init-directory $EMACS_D \
        --eval \"(setq backpack-e2e--results-file \\\"$RESULTS_FILE\\\")\" \
        --eval \"(setq backpack-e2e--generate-only t)\" \
        -l $EMACS_D/test/e2e-treesit.el \
        -f backpack-e2e--run-and-exit" /dev/null
    TEST_EXIT=1
fi

exit $TEST_EXIT
