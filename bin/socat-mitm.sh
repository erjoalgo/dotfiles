#!/bin/bash -x

set -euo pipefail

VERBOSE_OPT=-v
PROTOCOL=TCP
while getopts "huq" OPT; do
    case ${OPT} in
        q)
            # quiet
            VERBOSE_OPT=
            ;;
        u)
            PROTOCOL=UDP
            ;;
        h)
            less $0
            exit 0
            ;;
    esac
done
shift $((OPTIND -1))

LISTEN_PORT=${1} && shift
REMOTE_ADDRESS=${1} && shift

sudo socat ${VERBOSE_OPT} \
     ${PROTOCOL}-LISTEN:${LISTEN_PORT},fork,reuseaddr  \
     ${PROTOCOL}:${REMOTE_ADDRESS}
