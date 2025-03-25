#!/bin/bash -x

set -euo pipefail

while getopts "r:d:s:t:h" OPT; do
    case ${OPT} in
        # required
        r)
            RTSP_URL=${OPTARG}
            ;;
        d)
            DIRECTORY=${OPTARG}
            ;;
        # optional
        t)
            # stream title
            TITLE=${OPTARG}
            ;;
        s)
            DELAY_SECONDS=${OPTARG}
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

if test -n "${TITLE:-}" -a -z "${DIRECTORY:-}"; then
    DIRECTORY="${HOME}/pictures/rtsp-snaps/${TITLE}"
    mkdir -p ${DIRECTORY}
fi

test -n "${RTSP_URL:-}"
test -d "${DIRECTORY:-}"
TITLE=${TITLE:-snapshot}
DELAY_SECONDS=${DELAY_SECONDS:-60}

function timestamp {
    date +%s
}

function rtsp-snap {
    RTSP_URL=${1} && shift
    OUTPUT=${1} && shift
    # https://superuser.com/questions/1792249/
    # ffmpeg-suppress-warning-when-writing-to-a-single-image
    ffmpeg -y -rtsp_transport tcp -i "${RTSP_URL}" -frames:v 1 -update true "${OUTPUT}"
}

while true; do
    TIME_BEFORE=$(timestamp)
    OUTPUT="${DIRECTORY}/${TITLE}-${TIME_BEFORE}.jpeg"
    if ! rtsp-snap "${RTSP_URL}" "${OUTPUT}"; then
        echo "snapshot failed!"
    fi
    ELAPSED=$(($(timestamp) - ${TIME_BEFORE}))
    REMAINING=$((${DELAY_SECONDS} - ${ELAPSED}))
    if test ${REMAINING} -lt 0; then
        REMAINING=0
    fi
    echo "snapshot took ${ELAPSED}s, sleeping for another ${REMAINING}s"
    sleep ${REMAINING}
done
