#!/bin/bash -x

set -euo pipefail

while getopts "u:a:" OPT; do
    case ${OPT} in
    u)
        UNLOAD=${OPTARG}
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))

if test -z "${UNLOAD:-}"; then
    pactl load-module module-null-sink sink_name=test
else
    IDX=$(pactl list short modules | grep module-loopback | cut -f1)
    pactl unload-module "${IDX}"
fi
