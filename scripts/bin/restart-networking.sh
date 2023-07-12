#!/bin/bash -x

set -euo pipefail

sudo service networking stop
sudo rm /var/lib/dhcp/*lease*
while sudo pkill dhclient; do
    sleep 1
done

IFACES=$(ip link | grep -Po '^[0-9]+: [^:]+' | cut -f2 -d' ')
for IFACE in ${IFACES}; do
    sudo ip addr flush ${IFACE}
done

echo "ip addr after flushing: "
sudo ip addr

service networking restart
