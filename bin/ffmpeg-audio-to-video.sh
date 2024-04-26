#!/bin/bash -x

set -euo pipefail

# https://superuser.com/questions/1041816/

while getopts "a:i:o:h" OPT; do
    case ${OPT} in
    a)
        AUDIO=${OPTARG}
        ;;
    i)
        IMAGE=${OPTARG}
        ;;
    o)
        # optional
        OUTPUT=${OPTARG}
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

test -e "${IMAGE}"
test -e "${AUDIO}"
OUTPUT=${OUTPUT:-${AUDIO}.mp4}

ffmpeg -loop 1 -i "${IMAGE}" -i "${AUDIO}" -shortest "${OUTPUT}"
