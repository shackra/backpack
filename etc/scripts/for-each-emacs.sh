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
    echo "cannot put artifacts at $2, does not exist."
    exit 1
fi


for the_emacs in $EMACS_VERSIONS_TO_TEST; do
    prepare-and-run $the_emacs $1 $2
done
