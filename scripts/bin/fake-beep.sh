#!/bin/bash -x

set -euo pipefail
while getopts "f:d:h:" OPT; do
    case ${OPT} in
    f)
        FREQ=${OPTARG}
        ;;
    d)
        DURATION_SECONDS=${OPTARG}
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))


FREQ=${FREQ:-440}
DURATION_SECONDS=${DURATION_SECONDS:-1}

# https://www.baeldung.com/linux/pc-speaker-beep-in-linux

speaker-test -t sine -f "${FREQ}" -l 1 & sleep "${DURATION_SECONDS}" \
    && kill -9 $!
