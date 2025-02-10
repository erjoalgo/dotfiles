#!/bin/bash -x

set -euo pipefail

while getopts "f:t:h" OPT; do
    case ${OPT} in
    f)
        FILENAME=${OPTARG}
        ;;
    t)
        TSV_UPDATES=${OPTARG}
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

test -e "${FILENAME}"
test -e "${TSV_UPDATES}"

COPY="${FILENAME}.copy"
rm -f "${COPY}"
cp "${FILENAME}" "${COPY}"

OLDIFS=$IFS
IFS=$'\n'
for LINE in $(cat "${TSV_UPDATES}"); do
    TAG=$(cut -f1 <<< "${LINE}")
    VALUE=$(cut -f2 <<< "${LINE}")
    for IFD in 0 EXIF; do
        exif --tag="${TAG}" "--ifd=${IFD}" --set-value="${VALUE}" "${COPY}" --output "${COPY}" || true
    done
done
