#!/bin/bash -x

set -euo pipefail

# IFACE=$(networksetup -listnetworkserviceorder | grep -oP '(?<=Wi-Fi, Device: )[^)]+')
IFACE=$(networksetup -listnetworkserviceorder | grep -o 'Wi-Fi, Device:[^)]*' | cut -f2 -d: | tr -d ' ')

STATE=$(/usr/sbin/networksetup -getairportpower ${IFACE} | grep -o 'On\|Off')

NEW_STATE=$(test $STATE = "On"  && echo "Off" || echo "On")

say "turning wifi ${NEW_STATE}"

/usr/sbin/networksetup -setairportpower ${IFACE} ${NEW_STATE}


if test "On" = "${NEW_STATE}"; then
    while ! /usr/sbin/networksetup -getairportnetwork ${IFACE}; do
        sleep 1
        say "waiting for connection"
    done

    ESSID=$(/usr/sbin/networksetup -getairportnetwork ${IFACE} | cut -d: -f2 | xargs echo)
    say "connected to ${ESSID}"
    IP_ADDR=$(ifconfig ${IFACE} | grep -o "inet [0-9.]*")
    say "my ip address is ${IP_ADDR}"
fi
