#!/bin/bash -x

set -euo pipefail

while pgrep pulseaudio; do
    pulseaudio --kill || true
    pkill pulseaudio || true
    sleep 1
done

pulseaudio --start

BEEP=/usr/share/sounds/beep.wav

if ! test -e "${BEEP}"; then
    sudo $(which genbeepwav.py) 440:3000 --output "${BEEP}"
fi

aplay "${BEEP}"
