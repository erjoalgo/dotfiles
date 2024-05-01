#!/bin/bash -x

set -euo pipefail

while getopts "hn" OPT; do
    case ${OPT} in
    n)
        NONINTERACTIVE=true
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


SINKS=$(pactl list sinks | grep -Po '(?<=Name: ).+')
SAMPLE=$(pactl list samples  | grep -Po '(?<=Name: ).+' | head -1)

for SINK in ${SINKS}; do
    echo "trying sink: ${SINK}..."
    if pactl get-sink-mute "${SINK}" | grep "Mute: *yes"; then
        echo "unmuting sink ${SINK}..."
        pactl set-sink-mute "${SINK}" no
    fi
    pactl play-sample "${SAMPLE}" "${SINK}"
    if test "${NONINTERACTIVE:-}" != true; then
        read -p "continue? "
    fi
done
