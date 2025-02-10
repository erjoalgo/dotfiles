#!/bin/bash -x

set -euo pipefail

while getopts "i:f:o:b:z:d:ph" OPT; do
    case ${OPT} in
        # required
        i)
            INPUT_FILENAME=${OPTARG}
            ;;
        f)
            FOREGROUND_COLOR=${OPTARG}
            ;;
        # optional
        o)
            OUTPUT_FILENAME=${OPTARG}
            ;;
        b)
            ORIGINAL_FOREGROUND_COLOR=${OPTARG}
            ;;
        z)
            FUZZ=${OPTARG}
            ;;
        d)
            DENSITY=${OPTARG}
            ;;
        p)
            PRINT=true
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

test -e "${INPUT_FILENAME}"

BASE="${INPUT_FILENAME%.*}"
EXT="${INPUT_FILENAME##*.}"

ORIGINAL_FOREGROUND_COLOR=${ORIGINAL_FOREGROUND_COLOR:-black}
OUTPUT_FILENAME=${OUTPUT_FILENAME:-"${BASE}-${FOREGROUND_COLOR}.${EXT}"}
FUZZ=${FUZZ:-10}
DENSITY=${DENSITY:-200}


test -n "${FOREGROUND_COLOR}"

echo "chaning foreground color from ${ORIGINAL_FOREGROUND_COLOR} to ${FOREGROUND_COLOR}"
echo "input file is ${INPUT_FILENAME}"
echo "saving file as ${OUTPUT_FILENAME}"

convert -density "${DENSITY}" \
        "${INPUT_FILENAME}"  \
        -fill "${FOREGROUND_COLOR}" \
        -opaque "${ORIGINAL_FOREGROUND_COLOR}" \
        -fuzz "${FUZZ}%" \
        "${OUTPUT_FILENAME}"

x-www-browser-local-file.sh "${OUTPUT_FILENAME}"

if test "${PRINT:-}" = true; then
    read -p "confirm printing: "
    lpr-print.sh "${OUTPUT_FILENAME}"
fi
