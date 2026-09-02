#!/bin/bash -x

set -euo pipefail

DEVICE=${DEVICE:-/dev/sr0}

while getopts "d:p:" OPT; do
    case ${OPT} in
        d)
            DEVICE=${OPTARG}
            ;;
        p)
            OUTPUT_PATH=${OPTARG}
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

test -n "${OUTPUT_PATH:-}"

TMP=$(mktemp -d)

sudo mount -oloop "${DEVICE}" "${TMP}"

mkdir -p "${OUTPUT_PATH}"

rsync -arv "${TMP}" "${OUTPUT_PATH}"

eject
