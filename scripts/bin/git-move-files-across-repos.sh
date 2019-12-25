#!/bin/bash -x

set -euo pipefail

SOURCE_REPO=$(pwd)

while getopts "s:d:h" OPT; do
    case ${OPT} in
    d)
        DEST_REPO=${OPTARG}
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))

# based on https://stackoverflow.com/a/11426261/1941755
PATCH=$(tempfile)

cd ${SOURCE_REPO}
FILES=${*}
git log --pretty=email --patch-with-stat --reverse --full-index --binary -- \
    ${FILES} > ${PATCH}
less ${PATCH}
read -p "confirm patch: "

cd ${DEST_REPO}
git am < ${PATCH}
