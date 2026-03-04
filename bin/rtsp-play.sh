#!/bin/bash -x

set -euo pipefail

RTSP_URL=${1} && shift

if ! command -v ffplay; then
    sudo apt-get install -y ffmpeg
fi

ffplay -rtsp_transport tcp "${RTSP_URL}"
