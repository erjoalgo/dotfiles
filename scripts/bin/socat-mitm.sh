#!/bin/bash -x

set -euo pipefail

while getopts "h" OPT; do
    case ${OPT} in
        h)
            less $0
            exit 0
            ;;
    esac
done
shift $((OPTIND -1))

LISTEN_PORT=${1} && shift
REMOTE_ADDRESS=${1} && shift

sudo socat -v TCP-LISTEN:${LISTEN_PORT},fork,reuseaddr TCP:${REMOTE_ADDRESS}
