#!/bin/bash -x

set -euo pipefail

while getopts "hm:n:" OPT; do
    case ${OPT} in
    m)
        MINUTES=${OPTARG}
        ;;
    n)
        NAME=${OPTARG}
        ;;
    t)
        TIMESPEC=${OPTARG}
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))

if test -n "${MINUTES:-}"; then
    TIMESPEC="NOW + ${MINUTES}"
fi

at ${TIMESPEC} minutes <<EOF
alarm-now -n "${NAME}"
EOF
