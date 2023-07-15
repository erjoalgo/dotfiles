#!/bin/bash -x

set -euo pipefail


while getopts "h:s:o:" OPT; do
    case ${OPT} in
        s)
            # source or local interface
            IFACE_SOURCE=${OPTARG}
            ;;
        o)
            # internet-connected interface
            IFACE_OUT=${OPTARG}
            ;;
        h)
            less $0
            exit 0
            ;;
    esac
done
shift $((OPTIND -1))

sudo sysctl -w net.ipv4.ip_forward=1

sudo iptables -I FORWARDING 1 -s ${IFACE_SOURCE} -o ${IFACE_OUT} -j ACCEPT
sudo iptables -t nat -I POSTROUTING 1 -o ${IFACE_SOURCE} -j MASQUERADE
