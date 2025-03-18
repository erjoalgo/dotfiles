#!/bin/bash -x

set -euo pipefail

RTSP_URL=${1} && shift

ffplay -rtsp_transport tcp "${RTSP_URL}"
