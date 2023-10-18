#!/bin/bash -x

set -euo pipefail

VERBOSE_OPT=-v
while getopts "hq" OPT; do
    case ${OPT} in
        q)
            # quiet
            VERBOSE_OPT=
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
     TCP-LISTEN:${LISTEN_PORT},fork,reuseaddr  \
     TCP:${REMOTE_ADDRESS}
