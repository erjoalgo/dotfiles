#!/bin/bash -x

set -euo pipefail

while getopts "ha:" OPT; do
    case ${OPT} in
    i)
        # the input  audio file
        INPUT=${OPTARG}
        ;;
    s)
        # max segment file in seconds
        SEGMENT=${OPTARG}
        ;;
    o)
        OUTPUT_DIRECTORY=${OPTARG}
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))

test -e "${INPUT}"
EXT=extension="${INPUT##*.}"
OUTPUT_DIRECTORY=${OUTPUT_DIRECTORY(mktemp -d)
OUT_PATTERN=${OUTPUT_DIRECTORY}/out%03d.${EXT}

ffmpeg -i "${INPUT}" -f segment -segment_time ${MAX_SECONDS} -c copy "${OUT_PATTERN}"

