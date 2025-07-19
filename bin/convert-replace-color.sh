#!/bin/bash -x

set -euo pipefail

FUZZ_PERCENT=10
ORIGINAL_COLOR=white

while getopts "i:o:c:w:z:h" OPT; do
    case ${OPT} in
    i)
        INPUT=${OPTARG}
        ;;
    o)
        OUTPUT=${OPTARG}
        ;;
    w)
        # old color
        ORIGINAL_COLOR=white
        ;;
    c)
        # new color
        COLOR=${OPTARG}
        ;;
    z)
        FUZZ_PERCENT=${OPTARG}
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


test -e "${INPUT}"

if test "${COLOR}" = alpha; then
    MASK=$(mktemp /tmp/XXXXXXXX.png)
    convert "${INPUT}" -colorspace HSB -separate -negate "${MASK}"
    MASK_2="${MASK%.*}-2.png"
    test -e "${MASK_2}"
    convert "${INPUT}" -alpha Off "${MASK_2}" -compose CopyOpacity -composite "PNG32:${OUTPUT}"
    qimgv "${OUTPUT}"
else
    convert "${INPUT}" -fuzz "${FUZZ_PERCENT}%"  \
            -fill "${COLOR}" \
            -opaque "${ORIGINAL_COLOR}" \
            -flatten \
            "${OUTPUT}"
fi


qimgv "${OUTPUT}"
