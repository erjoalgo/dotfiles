#!/bin/bash -x

set -euo pipefail

# Execute a command and pipe its output to a while loop
sudo dmesg -W | while IFS=$'\n' read -r LINE; do
    echo "Processing line: $LINE"
    if grep -F "now running tv power..." <<< "${LINE}"; then
        IR_REMOTE=/home/ealfonso/.stumpwmrc.d/bin/ir-remote.py
        if ! { "${IR_REMOTE}" -b LG_POWER,TCL_ENTER ||
                   curl -s localhost:2727/LG_POWER,TCL_ENTER ; } ; then
            echo "IR remote failed!"
        fi
    fi
done
