#!/bin/bash -x

set -euo pipefail

function usage {
    echo "usage: mic-echo.sh <on|off>"
}

STATUS="${1:-}" && shift || usage

while getopts "h" OPT; do
    case ${OPT} in
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))

if test "${STATUS:-}" = on; then
    pactl load-module module-null-sink sink_name=test
elif test "${STATUS:-}" = off; then
    IDX=$(pactl list short modules | grep module-loopback | cut -f1)
    pactl unload-module "${IDX}"
else
    usage
    exit ${LINENO}
fi
