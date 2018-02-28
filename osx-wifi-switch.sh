#!/bin/bash -x

set -euo pipefail

# IFACE=$(networksetup -listnetworkserviceorder | grep -oP '(?<=Wi-Fi, Device: )[^)]+')
IFACE=$(networksetup -listnetworkserviceorder | grep -o 'Wi-Fi, Device:[^)]*' | cut -f2 -d: | tr -d ' ')

STATE=$(/usr/sbin/networksetup -getairportpower ${IFACE} | grep -o 'On\|Off')

NEW_STATE=$(test $STATE = "On"  && echo "Off" || echo "On")

/usr/sbin/networksetup -setairportpower ${IFACE} ${NEW_STATE}
