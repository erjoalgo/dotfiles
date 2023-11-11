#!/bin/bash -x

set -euo pipefail


while getopts "i:x:sntThg:" OPT; do
    case ${OPT} in
    i)
        IFACE=${OPTARG}
        ;;
    s)
        WRITE_SYSTEMD_SERVICE=true
        ;;
    x)
        PXE_FILENAME=${OPTARG}
        ;;
    g)
        GATEWAY_IFACE=${OPTARG}
        ;;
    n)
        NO_SHARE_INTERNET=true
        ;;
    T)
        STATIC_IP_ADDRESS=true
        ;;
    t)
        USE_TMP_FILE=true
        ;;
    G)
        # use Google dns
        DNS=8.8.8.8
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))

DHCPD=/usr/sbin/dhcpd

function find-iface {
    echo "select target iface: " 1>&2
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


if test -n "${USE_TMP_FILE:-}" && false; then
    TMP=/tmp/dhcpd/
    mkdir -p "${TMP}"
    CONF=$(sudo mktemp -p "${TMP}")
    TRACE_FILE_OPT="-tf $(sudo mktemp -p "${TMP}")"
    LEASE_FILE_OPT="-lf $(sudo mktemp -p "${TMP}")"
    # TODO factor-out to apparmor permission installer script

    APPARMOR_LOCAL=/etc/apparmor.d/local/usr.sbin.dhcpd
    if ! test -e "${APPARMOR_LOCAL}"; then
        cat <<EOF | sudo tee "${APPARMOR_LOCAL}"
owner ${TMP}/** lrw
EOF
        sudo sed -i 's|# *\(include <local/usr.sbin.dhcpd>\)|\1|'  \
             /etc/apparmor.d/usr.sbin.dhcpd
        sudo service apparmor restart
    fi
else
    CONF=/etc/dhcp/dhcpd-${IFACE}.conf
    LEASE_FILE_OPT=""
    TRACE_FILE_OPT=""
fi


IFACE=${IFACE:-$(find-iface)}
IFACE_ID=$(python3 -c "print (0x$(md5sum <<<$IFACE | cut -f1 -d' ') % 10)")
PREFIX=10.0.${IFACE_ID}
# PXE_FILENAME=${PXE_FILENAME:-"pxelinux.0"}
REAL_ROUTER=${REAL_ROUTER:-$(find-route)}
IP_ADDR=${PREFIX}.1
SUBNET_CIDR=24
DNS=${DNS:-$(find-dns)}

VBOX_CONF=/etc/vbox/networks.conf

if test -e "${VBOX_CONF}"; then
    sudo insert-text-block  \
         '# b53ee337-a469-4b65-a3b4-348b593b2178-allowlist-vbox-host-only-adapter-prefix' \
         "${VBOX_CONF}" <<EOF
* ${IP_ADDR}/${SUBNET_CIDR}
EOF
fi

sudo apt-get install -y iptables isc-dhcp-server

if test "${NO_SHARE_INTERNET:-}" != true; then
    sudo sysctl -w net.ipv4.ip_forward=1
    for GATEWAY_IFACE in ${GATEWAY_IFACE:-$(ip route | grep '^default' | grep -Po "(?<= dev) [^ ]+")}; do
        sudo iptables -I FORWARD 1 -i ${IFACE} -o ${GATEWAY_IFACE} -j NFLOG
        sudo iptables -I FORWARD 2 -i ${IFACE} -o ${GATEWAY_IFACE} -j ACCEPT
        sudo iptables -t nat -I POSTROUTING 1 -o ${IFACE} -j MASQUERADE
    done
fi

sudo sysctl -w net.ipv4.ip_forward=1

if test -z "${STATIC_IP_ADDRESS:-}"; then
    sudo ip link set ${IFACE} down
    sudo ip addr flush dev ${IFACE}
    sudo ip addr add ${IP_ADDR}/${SUBNET_CIDR} dev ${IFACE} \
       valid_lft forever preferred_lft forever
    sudo ip link set ${IFACE} up
else
    IP_ADDR=$(ip a show dev ${IFACE} | grep -Po '(?<=inet )[0-9.]+' | head -1)
fi


if test -n "${PXE_FILENAME:-}"; then
    PXE_OPT=$(cat<<EOF
  next-server ${IP_ADDR};
  filename "${PXE_FILENAME}";
EOF
)
else
    PXE_OPT=""
fi

sudo insert-text-block \
     '# 44ee01ca-8a56-411a-b047-f525e30a138a-dhcpd-conf' \
     "${CONF}" -e <<EOF
subnet ${PREFIX}.0 netmask 255.255.255.0 {
  range ${PREFIX}.2 ${PREFIX}.150;
  option broadcast-address ${PREFIX}.255;
  option routers ${IP_ADDR};
  default-lease-time 600;
  max-lease-time 7200;

  option subnet-mask 255.255.255.0;
  option domain-name ".local";
  option domain-name-servers ${DNS};

  ${PXE_OPT}
}

# No DHCP service in DMZ network (192.168.1.0/24)
subnet ${REAL_ROUTER%.*}.0 netmask 255.255.255.0 {}

INTERFACESv4=${IFACE};
EOF

while pgrep -f /usr/sbin/dhcpd | xargs kill -9 2>/dev/null; do
    sleep 1
done


sudo ip link set ${IFACE} up

if test -n "${USE_TMP_FILE:-}"; then
    sudo "${DHCPD}" -f -d -4  \
     -cf "${CONF}" "${IFACE}"  \
     ${LEASE_FILE_OPT} \
     ${TRACE_FILE_OPT}
elif test -n "${WRITE_SYSTEMD_SERVICE:-}"; then
    SERVICE_NAME=isc-dhcpd-server-${IFACE}

    echo "installing systemd service: ${SERVICE_NAME}"

    sudo tee /etc/network/interfaces.d/${IFACE}-static <<EOF
ifac     e wlx00c0cab2edfd inet static
    address ${IP_ADDR}
    netmask 255.255.255.0
EOF

    install-systemd-service.sh ${SERVICE_NAME} <<EOF
[Unit]
SourcePath=/etc/init.d/isc-dhcp-server
Description=DHCP server for ${IFACE}
Before=multi-user.target
Before=multi-user.target
Before=multi-user.target
Before=graphical.target
After=remote-fs.target
After=network-online.target
After=slapd.service
After=nss-lookup.target
Wants=network-online.target

[Service]
Restart=always
RestartSec=60
RemainAfterExit=no
ExecStart=$(which run-dhcpd.sh) -i ${IFACE} -t

[Install]
WantedBy=multi-user.target
EOF

    sudo service ${SERVICE_NAME} restart
else
    echo "either tmpfile or system service option must be provided" && exit ${LINENO}
fi
