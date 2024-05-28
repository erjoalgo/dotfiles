#!/bin/bash -x

set -euo pipefail

QUALITY=${QUALITY:-15}

while getopts "hl:" OPT; do
    case ${OPT} in
    l)
        QUALITY=${OPTARG}
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


for INPUT in $*; do
    OUTPUT=$(sed 's/[.]/.low-quality./' <<< "${INPUT}")
    convert "${INPUT}" -quality ${QUALITY} "${OUTPUT}"
    echo "wrote to ${OUTPUT} using quality ${QUALITY}"
done
