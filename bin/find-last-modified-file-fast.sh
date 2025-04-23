#!/bin/bash

set -euo pipefail

# from https://stackoverflow.com/a/7448828/1941755

# echo $0 ${*}
FIND_CMD=(find)
CUT_CMD=(cut -f5- -d' ')
DIR_PROVIDED=false
while getopts "he:d:xc:" OPT; do
    case ${OPT} in
    e)
        EXT="${OPTARG}"
        ;;
    d)
        if ! test -d "${OPTARG}"; then
            echo "WARN: skipping non-existent directory ${OPTARG}" 1>&2
            continue
        fi
        DIR=$(realpath "${OPTARG}")
        test -d "${DIR}"
        FIND_CMD+=("${DIR}")
        DIR_PROVIDED=true
        ;;
    c)
        # if -1, omits CMIN
        CMIN=${OPTARG}
        ;;
    x)
        # debug timestamps
        CUT_CMD=(cat)
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
    FIND_CMD+=(-iname "*.${EXT}")
fi

if test "${CMIN:-}" != -1; then
    FIND_CMD+=(-cmin -${CMIN:-100})
fi

FIND_CMD+=(-type f -exec stat --format '%Y :%y %n' "{}" \;)

"${FIND_CMD[@]}" | sort -nr | "${CUT_CMD[@]}"
