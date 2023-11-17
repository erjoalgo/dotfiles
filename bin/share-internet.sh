#!/bin/bash -x

set -euo pipefail


while getopts "s:o:ph" OPT; do
    case ${OPT} in
        s)
            # source or local interface
            IFACE_SOURCE=${OPTARG}
            ;;
        o)
            # internet-connected interface
            IFACE_OUT=${OPTARG}
            ;;
        p)
            PERSIST=true
            ;;
        h)
            less $0
            exit 0
            ;;
    esac
done
shift $((OPTIND -1))

sudo sysctl -w net.ipv4.ip_forward=1

set -x
for TARGET in NFLOG ACCEPT; do
    sudo iptables -I FORWARD 1 -i ${IFACE_SOURCE} -o ${IFACE_OUT} -j ${TARGET}
done
sudo iptables -t nat -I POSTROUTING 1 -o ${IFACE_OUT} -j MASQUERADE
sudo iptables -A FORWARD -i ${IFACE_OUT} -o ${IFACE_SOURCE} -m state --state ESTABLISHED,RELATED -j ACCEPT

if test -n "${PERSIST:-}"; then
    sudo apt-get install -y iptables-persistent
    IDENTIFIER="${IFACE_SOURCE}-to-${IFACE_OUT}"
    sudo iptables-save |  \
        sudo insert-text-block \
             "# b674161f-f554-4255-86cb-80065761abdc-share-internet-${IDENTIFIER}" \
             /etc/iptables/rules.v4
fi
