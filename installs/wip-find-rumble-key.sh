#!/bin/bash -x

set -euo pipefail

INI=/usr/share/games/mupen64plus/InputAutoCfg.ini

function rumble-detected {
    OUTPUT=$(timeout 10 mupen64plus ~/Downloads/n64/Legend\ of\ Zelda,\ The\ -\ Ocarina\ of\ Time\ \(USA\)\ \(Rev\ 2\).z64 2>&1)
    if grep -F "Input Warning: Couldn't open rumble support for joystick #1" <<<  \
            "${OUTPUT}"; then
        echo "rumble not supported"
        return 1
    else
        echo "could not determine state of rumble support"
        return 0
    fi
}

for KEY in $(seq 400); do
    echo "trying key: ${KEY}"
    sudo sed -i  \
        "s/^Rumblepak switch.*/Rumblepak switch = key(${KEY})/g" \
        "${INI}"
    sudo grep -F "Rumblepak switch = key(${KEY})" "${INI}"
    if rumble-detected; then
        break
    fi
    sleep 1
done
