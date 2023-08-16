#!/bin/bash -x

set -euo pipefail

while getopts "hi:r" OPT; do
    case ${OPT} in
    i)
        WIFI_IFACE=${OPTARG}
        ;;
    r)
        RESET=true
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))

WIFI_IFACE=${WIFI_IFACE:-$(ip l | grep -P '^[0-9]' |  \
                               cut -f2 -d: | tr -d ' ' | grep '^w')}

sudo nmcli radio wifi off


if test -n "${RESET:-}"; then
    MACCHANGER_OPT=-p
    NEW_HOSTNAME="$(hostname)"
else
    MACCHANGER_OPT=-r
    NEW_HOSTNAME="$(hostname)-${RANDOM}"
fi


sudo macchanger ${MACCHANGER_OPT} "${WIFI_IFACE}"

sudo sed -i "s/\(send host-name = \).*/\1\"${NEW_HOSTNAME}\"/" \
     /etc/dhcp/dhclient.conf

sudo nmcli radio wifi on

