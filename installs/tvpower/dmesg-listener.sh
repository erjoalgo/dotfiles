#!/bin/bash -x

set -euo pipefail

# Execute a command and pipe its output to a while loop
sudo dmesg -W | while IFS=$'\n' read -r LINE; do
    echo "Processing line: $LINE"
    if grep -F "now running tv power..." <<< "${LINE}"; then
        /home/ealfonso/.stumpwmrc.d/bin/ir-remote.py -b LG_POWER,TCL_ENTER
    fi
done
