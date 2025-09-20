IFS=' '

if [ ! -n "${EMACS_VERSIONS_TO_TEST+x}" ]; then
    echo "EMACS_VERSIONS_TO_TEST is not defined"
    exit 1
fi

if [ ! -d "$1" ]; then
  echo "cannot clone $1, does not exist."
  exit 1
fi

if [ ! -d "$2" ]; then
    echo "test output will be printed to console..."
fi


for the_emacs in $EMACS_VERSIONS_TO_TEST; do
    if [ -d "$2" ]; then
	prepare-and-run $the_emacs $1 > $2/$the_emacs-output.log 2>&1
    else
	prepare-and-run $the_emacs $1
    fi
done
