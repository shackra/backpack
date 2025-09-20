if [ ! -d "$2" ]; then
  echo "cannot clone $2, does not exist."
  exit 1
fi

if [ ! -d "$3" ]; then
    echo "cannot put artifacts at $3, does not exist."
    exit 1
fi

if ! command -v $1 >/dev/null 2>&1
then
    echo "$1 could not be found"
    exit 1
fi

TEST_HOME=$(mktemp -d)
git clone --recurse-submodules $2 $TEST_HOME/.emacs.d >/dev/null 2>&1

$1 --version | head -n 1 > "$3/$1-output.log"

HOME=$TEST_HOME $1 -batch -l ert -l $TEST_HOME/.emacs.d/test/all-tests.el -f ert-run-tests-batch-and-exit >> "$3/$1-output.log" 2>&1

rm -rf $TEST_HOME
