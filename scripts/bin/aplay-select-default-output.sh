#!/bin/bash

set -euo pipefail

DEVICES=$(aplay -l | grep ^card | sed 's/^card \([0-9]*\).*device \([0-9]*\).*/\1,\2/g')
TEST_WAV_SOUNDS=$(echo /usr/share/sounds/alsa/Front_{Center,Left,Right}.wav)

for DEVICE in ${DEVICES}; do
    for TEST_WAV in ${TEST_WAV_SOUNDS}; do
        DEVICE_SPEC="plughw:${DEVICE}"
        echo "on device ${DEVICE_SPEC}, playing ${TEST_WAV}"
        aplay -D ${DEVICE_SPEC} ${TEST_WAV} >& /dev/null
    done

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
done

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


