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
    echo "select target iface: "
    select IFACE in $(ip link | grep -Po '^[0-9]+: [^:]+' | cut -d' ' -f2); do
        break
    done
    echo "${IFACE}"
}

function find-route {
    ip route get 8.8.8.8 | grep -Po '(?<=via )[0-9.]+'
}

function find-dns {
    grep -Po '(?<=^nameserver )[0-9.]+' /etc/resolv.conf | head -1
}

CONF=$(sudo mktemp)
IFACE=${IFACE:-$(find-iface)}
PREFIX=10.0.0
PXE_FILENAME=${PXE_FILENAME:-"pxelinux.0"}
REAL_ROUTER=${REAL_ROUTER:-$(find-route)}
IP_ADDR=${PREFIX}.1
SUBNET_CIDR=24
DNS=${DNS:-$(find-dns)}

sudo insert-text-block  \
     '# b53ee337-a469-4b65-a3b4-348b593b2178-allowlist-vbox-host-only-adapter-prefix' \
      /etc/vbox/networks.conf <<EOF
* ${IP_ADDR}/${SUBNET_CIDR}
EOF

sudo iptables -P FORWARD ACCEPT # TODO fix this, make this more specific
for GATEWAY_IFACE in $(ip route | grep '^default' | grep -Po "(?<= dev .*)"); do
    sudo iptables -t nat -A POSTROUTING -o ${GATEWAY_IFACE} -j MASQUERADE
done

sudo sysctl -w net.ipv4.ip_forward=1

while ! ip -f inet addr show ${IFACE}  | grep "inet ${IP_ADDR}/${SUBNET_CIDR}"; do
    sudo ip link set ${IFACE} down
    sudo ip addr flush dev ${IFACE}
    sudo ip addr add ${IP_ADDR}/${SUBNET_CIDR} dev ${IFACE} \
       valid_lft forever preferred_lft forever
    sudo ip link set ${IFACE} up
done


sudo insert-text-block \
     '# 44ee01ca-8a56-411a-b047-f525e30a138a-dhcpd-conf' \
     "${CONF}" -e <<EOF
default-lease-time 600;
max-lease-time 7200;


subnet 10.0.0.0 netmask 255.255.255.0 {
  range 10.0.0.1 10.0.0.20;


  option subnet-mask 255.255.255.0;
  option routers ${IP_ADDR};
  option broadcast-address ${REAL_ROUTER%.*}.255;
  option domain-name-servers ${DNS};

  next-server ${IP_ADDR};
  filename "${PXE_FILENAME}";
}

# No DHCP service in DMZ network (192.168.1.0/24)
subnet ${REAL_ROUTER%.*}.0 netmask 255.255.255.0 {}
EOF

LEASE_FILE=$(sudo mktemp)

while pgrep -f /usr/sbin/dhcpd | xargs kill -9 2>/dev/null; do
    sleep 1
done
TRACE_FILE=$(sudo mktemp)
echo "trace file: ${TRACE_FILE}"
sudo /usr/sbin/dhcpd -f -d -4 -cf "${CONF}" "${IFACE}"  \
     -lf "${LEASE_FILE}" -tf "${TRACE_FILE}"
