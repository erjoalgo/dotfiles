#!/bin/bash -x

set -euo pipefail

VERBOSE_OPT=-v
PROTOCOL=TCP
BIND_OPT=""
while getopts "h0uq" OPT; do
    case ${OPT} in
        q)
            # quiet
            VERBOSE_OPT=
            ;;
        u)
            PROTOCOL=UDP
            ;;
        0)
            BIND_OPT=",bind=0.0.0.0"
            ;;
        h)
            less $0
            exit 0
            ;;
    esac
done
shift $((OPTIND -1))

LISTEN_PORT=${1}${BIND_OPT} && shift
REMOTE_ADDRESS=${1} && shift

sudo socat ${VERBOSE_OPT} \
     ${PROTOCOL}-LISTEN:${LISTEN_PORT},fork,reuseaddr  \
     ${PROTOCOL}:${REMOTE_ADDRESS}
