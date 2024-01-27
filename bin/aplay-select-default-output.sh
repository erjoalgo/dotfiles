#!/bin/bash

set -euo pipefail

ON_FAILURE=ignore
while getopts "nd:eh" OPT; do
    case ${OPT} in
        n)
            NO_PROMPT=true
            ;;
        d)
            # e.g. "0,1" for plughw:0,1
            SELECTED_DEVICE=${OPTARG}
            ;;
        e)
            ON_FAILURE=exit
            ;;
        h)
            less $0
            exit 0
            ;;
    esac
done
shift $((OPTIND -1))

if test -z "${SELECTED_DEVICE:-}"; then
    DEVICES=$(aplay -L | grep -v '^ ')
    TEST_WAV_SOUNDS=$(echo /usr/share/sounds/alsa/Front_{Left,Right}.wav)

    for DEVICE in ${DEVICES}; do
        CMD=("speaker-test" "-D" "${DEVICE}" "-l1")
        CHANNEL_OPT=

        echo ""
        echo ""
        echo "Testing device ${DEVICE} via ${CMD[@]}"
        echo ""

        while true; do
            DONE=false
            for CHAN_OPT in "" -c0 -c1 -c2; do
                if ${CMD[@]} $CHAN_OPT; then
                    DONE=true
                    break
                fi
            done
            if test ${DONE} = true; then
                break
            else
                STATUS=$?
                echo "ERROR: failed to test ${DEVICE}"
                if test "${ON_FAILURE}" = kill-pulseaudio; then
                    pulseaudio -k || true
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
                    speaker-test -D ${DEVICE} -l0
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


