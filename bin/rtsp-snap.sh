#!/bin/bash -x

set -euo pipefail

while getopts "h" OPT; do
    case ${OPT} in
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

RTSP_URL=${1} && shift
OUTPUT=${1} && shift

ffmpeg -y -rtsp_transport tcp -i "${RTSP_URL}" -frames:v 1 "${OUTPUT}"
