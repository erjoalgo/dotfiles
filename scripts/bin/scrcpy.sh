#!/bin/bash -x

set -euo pipefail

# ACTION=="add", ENV{ID_SERIAL_SHORT}="...", RUN+="/home/ealfonso/.stumpwmrc.d/scripts/bin/scrcpy.sh"


# TODO make this into a udev script wrapper
LOG=/tmp/udev.log

exec 2>&1
exec >> "${LOG}"

export DISPLAY=:0.0
export XAUTHORITY=/home/ealfonso/.Xauthority

echo
env
echo

sleep 2

/usr/local/bin/scrcpy
