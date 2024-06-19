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

BASENAME="share-internet-${IFACE_SOURCE}-to-${IFACE_OUT}"
if test -n "${PERSIST:-}"; then
    sudo apt-get install -y iptables-persistent
    RULES_FILE="/etc/iptables/${BASENAME}"
else
    RULES_FILE="/tmp/${BASENAME}"
fi


sudo insert-text-block '# 48adcfbc-82d3-4d35-aaab-3ede23c1cad9-iptables-share-internet'  \
     "${RULES_FILE}"<<EOF
*filter
:INPUT ACCEPT [0:0]
:FORWARD ACCEPT [0:0]
:OUTPUT ACCEPT [0:0]
-A FORWARD -i ${IFACE_SOURCE} -o ${IFACE_OUT} -j ACCEPT
-A FORWARD -i ${IFACE_SOURCE} -o ${IFACE_OUT} -j NFLOG
-A FORWARD -i ${IFACE_OUT} -o ${IFACE_SOURCE} -m state --state RELATED,ESTABLISHED -j ACCEPT
COMMIT
*nat
:PREROUTING ACCEPT [0:0]
:INPUT ACCEPT [0:0]
:OUTPUT ACCEPT [0:0]
:POSTROUTING ACCEPT [0:0]
-A POSTROUTING -o ${IFACE_OUT} -j MASQUERADE
COMMIT
EOF

sudo iptables-restore "${RULES_FILE}"
