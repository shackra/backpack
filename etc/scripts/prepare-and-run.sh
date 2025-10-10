set -euo pipefail

if [ ! -d "$2" ]; then
  echo "cannot copy $2, does not exist."
  exit 1
fi

if ! command -v $1 >/dev/null 2>&1
then
    echo "$1 could not be found"
    exit 1
fi

TEST_HOME=$(mktemp -d)
rsync -a --filter=':- .gitignore' --filter='- .git/' --filter='- .github/' $2 "$TEST_HOME/.emacs.d"

$1 --version | head -n 1

$1 --init-directory $TEST_HOME/.emacs.d -batch -l ert -l $TEST_HOME/.emacs.d/test/all-tests.el -f ert-run-tests-batch-and-exit

rm -rf $TEST_HOME
