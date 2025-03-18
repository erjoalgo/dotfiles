#!/bin/bash

set -euo pipefail

while getopts "d:t:h" OPT; do
    case ${OPT} in
    d)
        # stls directory
        STLS=${OPTARG}
        ;;
    # optional
    t)
        # thumbs directory
        THUMBS=${OPTARG}
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

STLS="/afs/asus.erjoalgo.com/home/ealfonso/git/3d/"
STLS=${STLS:-.}
FILES=$(find "${STLS}" -iname '*stl')
THUMBS=${THUMBS:-"${STLS}/.thumbs"}
IFS=$'\n'
CNT=0
mkdir -p "${THUMBS}"
for FILE in ${FILES}; do
    PNG="${THUMBS}/$(basename ${FILE}).png"
    if test -e "${PNG}" -a "${PNG}" -nt "${FILE}"; then
        echo "skipping existing and current thumbnail: ${PNG}"
        continue
    fi
    openscad -o "${PNG}" <(echo "import(\"${FILE}\");")
    echo "wrote thumb to ${PNG}"
    CNT=$((CNT + 1))
done

echo "wrote ${CNT} thumbs to ${THUMBS}"

if command -v imageoverview.py; then
    imageoverview.py -d "${THUMBS}"
fi
