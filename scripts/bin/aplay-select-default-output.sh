#!/bin/bash

set -euo pipefail

ON_FAILURE=exit
while getopts "nd:ih" OPT; do
    case ${OPT} in
        n)
            NO_PROMPT=true
            ;;
        d)
            # e.g. "0,1" for plughw:0,1
            SELECTED_DEVICE=${OPTARG}
            ;;
        i)
            ON_FAILURE=ignore
            ;;
        h)
            less $0
            exit 0
            ;;
    esac
done
shift $((OPTIND -1))

if test -z "${SELECTED_DEVICE:-}"; then
    DEVICES=$(aplay -l | grep ^card | sed 's/^card \([0-9]*\).*device \([0-9]*\).*/\1,\2/g')
    TEST_WAV_SOUNDS=$(echo /usr/share/sounds/alsa/Front_{Left,Right}.wav)

    for DEVICE in ${DEVICES}; do
        DEVICE_SPEC="plughw:${DEVICE}"
        CMD="speaker-test -D ${DEVICE_SPEC} -l1"

        echo ""
        echo ""
        echo "Testing device ${DEVICE} via ${CMD}"
        echo ""

        while true; do
            if ${CMD}; then
                break
            else
                STATUS=$?
                echo "ERROR: failed to test ${DEVICE_SPEC}"
                if test "${ON_FAILURE}" = kill-pulseaudio; then
                    pulseaudio -k
                elif test "${ON_FAILURE}" = exit; then
                    exit ${STATUS}
                elif test "${ON_FAILURE}" = ignore; then
                    STATUS=0
                    break
                else
                    echo "unknown on-failure action: ${ON_FAILURE}"
                    exit ${LINENO}
                fi
            fi
        done

        if test -z "${NO_PROMPT:-}"; then
            read -p"(s)elect, s(k)ip (q)uit, (i)nfloop: " OPT
            case "${OPT}" in
                s)
                    SELECTED_DEVICE=${DEVICE}
                    break
                    ;;
                k)
                    ;;
                i)
                    speaker-test -D ${DEVICE_SPEC} -l0
                    ;;
                q)
                    exit 0
                    ;;
                *)
                    echo "unrecognized input"
                    exit ${LINENO}
                    ;;
            esac
        fi
    done
fi


if test -n "${SELECTED_DEVICE:-}"; then
    DEVICE_SPEC="hw:${SELECTED_DEVICE}"
    # sink name should have no special chars
    SINK_NAME="alsa$(sed s/[,:]//g <<< ${DEVICE_SPEC})"

    if ! pacmd help > /dev/null; then
        pulseaudio --start
    fi

    # TODO check if module loaded
    pactl unload-module module-alsa-sink || true
    pactl load-module module-alsa-sink "device=${DEVICE_SPEC}" "sink_name=${SINK_NAME}"
    pactl set-default-sink ${SINK_NAME}

    pactl upload-sample /usr/share/sounds/alsa/Front_Right.wav sample

    # sometimes the first sample does not play
    for _ in $(seq 3); do
        pactl play-sample sample "@DEFAULT_SINK@"
        sleep 1
    done
fi


