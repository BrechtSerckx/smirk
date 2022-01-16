#! /usr/bin/env nix-shell
#! nix-shell -i bash -p coreutils cmake
# shellcheck disable=SC3040

set -eu -o pipefail

if [ ${1-} = "-d" ]; then
    if [ -n $2 ]; then
        DIR=$2
        shift; shift
        ln -sf "${PWD}/Makefile" "${DIR}/Makefile"
    else
        echo "ERROR: no project directory given"
    fi
else
    echo "Using 'PROJECT/arduino' as project directory"
    DIR="${PWD}"
fi
echo "Building ${DIR}"
( cd "${DIR}" && make "$@" )
