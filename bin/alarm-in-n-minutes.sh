#!/bin/bash -x

set -euo pipefail

while getopts "hm:n:t:" OPT; do
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
        less "$0"
        exit 0
        ;;
    *)
        echo "unrecognized flag: ${OPT}" && exit ${LINENO}
        ;;
    esac
done
shift $((OPTIND -1))

if test -n "${MINUTES:-}"; then
    TIMESPEC="NOW + ${MINUTES} minutes"
fi

at "${TIMESPEC}" <<EOF
alarm-now -n "${NAME}"
EOF
