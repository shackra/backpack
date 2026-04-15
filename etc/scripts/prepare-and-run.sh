set -euo pipefail

# prepare-and-run.sh -- Copy repo to tmpdir, run unit tests, clean up.
#
# Usage: prepare-and-run <emacs-binary> <source-dir>

TEST_HOME=$(prepare "$1" "$2")
trap "rm -rf $TEST_HOME" EXIT

$1 --init-directory $TEST_HOME/.emacs.d -batch -l $TEST_HOME/.emacs.d/early-init.el -l ert -l $TEST_HOME/.emacs.d/test/all-tests.el -f ert-run-tests-batch-and-exit
