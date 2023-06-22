#!/bin/bash -x

set -euo pipefail


while getopts "hi:x:" OPT; do
    case ${OPT} in
    i)
        IFACE=${OPTARG}
        ;;
    x)
        PXE_FILENAME=${OPTARG}
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))

function find-iface {
    ip link | grep -Po '^[0-9]+: [^:]+' | cut -d' ' -f2 |  \
        grep -P "enp|enx" | tail -1
}

CONF=$(sudo mktemp)
IFACE=${IFACE:-$(find-iface)}
PREFIX=10.0.0
PXE_FILENAME=${PXE_FILENAME:-"pxelinux.0"}

IP_ADDR=${PREFIX}.1

while ! ip -f inet addr show ${IFACE}  | grep "inet ${IP_ADDR}/24"; do
    ip link set ${IFACE} down
    ip addr add ${IP_ADDR}/24 dev ${IFACE} \
       valid_lft forever preferred_lft forever
    ip link set ${IFACE} up
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

  next-server 10.0.0.1;
  filename "${PXE_FILENAME}"; # setting a default, might be wrong for "non defaults"
}

# No DHCP service in DMZ network (192.168.1.0/24)
subnet 192.168.1.0 netmask 255.255.255.0 {
}
EOF

LEASE_FILE=$(sudo mktemp)

while pgrep -f /usr/sbin/dhcpd | xargs kill -9 2>/dev/null; do
    sleep 1
done
/usr/sbin/dhcpd -f -d -4 -cf "${CONF}" "${IFACE}" -lf "${LEASE_FILE}"
