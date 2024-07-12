#!/bin/bash -x

set -euo pipefail

while getopts "d:h" OPT; do
    case ${OPT} in
    d)
        DIRECTORY=${OPTARG}
        ;;
    h)
        less "$0"
        exit 0
        ;;
    *)
        echo "unrecognized flag: ${OPT}" && exit ${LINENO}
        ;;
    esac
done
shift $((OPTIND -1))

DIRECTORY=${DIRECTORY:-${HOME}/git/3d}

test -d "${DIRECTORY}"

THUMBS="${DIRECTORY}/thumbs"
mkdir -p "${THUMBS}"

cd "${DIRECTORY}"

# adapted from https://3dprinting.stackexchange.com/a/19668/41041
for EXT in stl STL ; do
  for STL in *.$EXT  ; do
    BASENAME=$(basename "$STL" .$EXT)
    DEST="${THUMBS}/${BASENAME}.png"
    if test ! -e "${DEST}" -o "$STL" -nt "${DEST}" ; then
        pwd
        openscad -o "${DEST}" --imgsize=100,100 <(echo "import(\"${STL}\");")
    fi
  done
done
eog "${THUMBS}"
