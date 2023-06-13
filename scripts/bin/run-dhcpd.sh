#!/bin/bash -x

set -euo pipefail


CONF=/tmp/dhcpd.conf
IFACE=enx9405bb1175cc
PREFIX=10.0.0

sudo ip link set dev ${IFACE} up

IP_ADDR=${PREFIX}.1

while ! ip -f inet addr show ${IFACE}  | grep "inet ${IP_ADDR}/24"; do
    ifdown ${IFACE}
    ip addr add ${IP_ADDR}/24 dev ${IFACE} \
       valid_lft forever preferred_lft forever
done


sudo insert-text-block \
     '# 44ee01ca-8a56-411a-b047-f525e30a138a-dhcpd-conf' \
     "${CONF}" <<EOF
default-lease-time 600;
max-lease-time 7200;

option domain-name-servers 8.8.8.8;
option subnet-mask 255.255.255.0;
option routers 10.0.0.1;

# option broadcast-address 10.0.0.255;
subnet 10.0.0.0 netmask 255.255.255.0 {
       range 10.0.0.1 10.0.0.20;

       # next-server 10.0.0.1;
       # filename "pxelinux.0"; # setting a default, might be wrong for "non defaults"
}

# No DHCP service in DMZ network (192.168.1.0/24)
subnet 192.168.1.0 netmask 255.255.255.0 {
}
EOF

INTERFACES=enx9405bb1175cc
LEASE_FILE=$(mktemp)
/usr/sbin/dhcpd -f -d -4 -cf ${CONF} $INTERFACES -lf ${LEASE_FILE}
