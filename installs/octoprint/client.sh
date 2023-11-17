#!/bin/bash -x

set -euo pipefail

while getopts "r:h" OPT; do
    case ${OPT} in
    r)
        REMOTE_SERVER=${OPTARG}
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))

usbip-client.sh -r "${REMOTE_SERVER}" -f "serial converter"
