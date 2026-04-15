set -euo pipefail

# prepare.sh -- Copy the Backpack repo into a temporary test directory.
#
# Usage: prepare.sh <emacs-binary> <source-dir>
#
# Validates that the Emacs binary exists and the source directory is valid,
# then copies the repo to a temporary directory using rsync.
#
# Prints the path to TEST_HOME as the LAST line of stdout.
# Callers should capture it with: TEST_HOME=$(prepare.sh emacs /path/to/repo | tail -1)
# or better, parse only the last line.

if [ ! -d "$2" ]; then
  echo "cannot copy $2, does not exist." >&2
  exit 1
fi

if ! command -v $1 >/dev/null 2>&1
then
    echo "$1 could not be found" >&2
    exit 1
fi

TEST_HOME=$(mktemp -d)
rsync -a --filter=':- .gitignore' --filter='- .git/' --filter='- .github/' "$2/" "$TEST_HOME/.emacs.d"

$1 --version | head -n 1 >&2

echo "$TEST_HOME"
