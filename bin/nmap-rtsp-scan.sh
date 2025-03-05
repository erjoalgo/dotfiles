#!/bin/bash -x

set -euo pipefail
while getopts "hp:" OPT; do
    case ${OPT} in
    p)
        PORT=${OPTARG}
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

CAMERA_IP=${1} && shift
PORT=${PORT:-554}
SCRIPT="$(realpath $(dirname "${BASH_SOURCE[0]}"))/rtsp-url-brute.nse"

nmap --script "${SCRIPT}" -p "${PORT}" "${CAMERA_IP}"
