#!/bin/bash -x

set -euo pipefail

WIFI_IFACE=${WIFI_IFACE:-$(ip l | grep -P '^[0-9]' |  \
                               cut -f2 -d: | tr -d ' ' | grep '^w')}

sudo nmcli radio wifi off
sudo macchanger -r ${WIFI_IFACE}

FAKE_HOSTANME=$(hostname)-${RANDOM}

sudo sed -i "s/\(send host-name = \).*/\1\"${FAKE_HOSTANME}\"/" \
     /etc/dhcp/dhclient.conf

sudo nmcli radio wifi on

