#! /usr/bin/env nix-shell
#! nix-shell -i bash -p coreutils cmake
# shellcheck disable=SC3040

set -eu -o pipefail

if [ -z "$1" ]; then
   echo "No project directory given"
   DIR=${PWD}
else
    DIR=$1
    shift
    ln -sf "${PWD}/Makefile" "${DIR}/Makefile"
fi
echo "Building ${DIR}"
( cd "${DIR}" && make "$@" )
