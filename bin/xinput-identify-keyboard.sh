#!/bin/bash -x

set -euo pipefail

LIST=$(xinput list)

OLDIFS=$IFS
IFS=$'\n'
for LINE in ${LIST}; do
    echo "listening to events from: ${LINE}"
    DEVICE_ID=$(grep -Po '(?<=id=)[0-9]+' <<< "${LINE}")
    if ! xinput --list-props "$DEVICE_ID" | grep -qP '^\s+Device Node.*/dev/input/event'; then
        continue
    fi
    xinput --test ${DEVICE_ID} &
    XINPUT_PID=$!
    trap -- "kill ${XINPUT_PID}" SIGINT
    wait $XINPUT_PID || true
done
