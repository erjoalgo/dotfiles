#!/bin/bash

set -euo pipefail

while getopts "nd:h" OPT; do
    case ${OPT} in
        n)
            NO_PROMPT=true
            ;;
        d)
            # e.g. "0,1" for plughw:0,1
            SELECTED_DEVICE=${OPTARG}
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
    DEVICES=$(aplay -L /3 | grep : | grep , | cut -f2 -d: | sort -u)

    TEST_WAV_SOUNDS=$(echo /usr/share/sounds/alsa/Front_{Left,Right}.wav)

    for DEVICE in ${DEVICES}; do
        echo "Testing device ${DEVICE}"
        DEVICE_SPEC="plughw:${DEVICE}"
        if ! speaker-test -D ${DEVICE_SPEC} -l1; then
            echo "ERROR: failed to test ${DEVICE_SPEC}"
            continue
        fi

        if test -z "${NO_PROMPT:-}"; then
            read -p"(s)elect, s(k)ip (q)uit: " OPT
            case "${OPT}" in
                s)
                    SELECTED_DEVICE=${DEVICE}
                    break
                    ;;
                k)
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
    sudo insert-text-block '# 9b5e7d87-021a-4223-add8-a7c8dc34af3a-select-default-alsa-device'  \
         /etc/asound.conf <<EOF
pcm.!default {
        type plug
        slave {
                pcm "hw:${SELECTED_DEVICE}"
        }
}
ctl.!default {
        type hw
        card $(cut -f1 -d, <<< "${SELECTED_DEVICE:-}")
}
EOF

    # read -p"confirm: "

    # not sure if all are needed to reload
    /etc/init.d/alsa-utils restart
    sudo alsactl restore
    # this tends to return non-zero exit on success...
    sudo alsactl init || true

    for WAV in ${TEST_WAV_SOUNDS}; do
        aplay ${WAV}
    done
fi


