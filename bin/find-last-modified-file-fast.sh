#!/bin/bash

set -euo pipefail

# from https://stackoverflow.com/a/7448828/1941755

# echo $0 ${*}
FIND_CMD=(find)
while getopts "he:d:" OPT; do
DIR_PROVIDED=false
    case ${OPT} in
    e)
        EXT="${OPTARG}"
        ;;
    d)
        DIR=$(realpath "${OPTARG}")
        test -d "${DIR}"
        FIND_CMD+=("${DIR}")
        DIR_PROVIDED=true
        ;;
    c)
        CMIN=${OPTARG}
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

if test "${DIR_PROVIDED:-}" != true; then
    echo "missing -d option" && exit ${LINENO}
fi

if test -n "${EXT:-}"; then
    FIND_CMD+=(-name "*.${EXT}")
fi

CMIN=${CMIN:-100}

FIND_CMD+=(-cmin -${CMIN} -type f \
                 -exec stat --format '%Y :%y %n' "{}" \;)

"${FIND_CMD[@]}" | \
    sort -nr |  \
    cut -d: -f2- |  \
    cut -f4- -d' '
