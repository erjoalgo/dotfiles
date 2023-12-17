#!/bin/bash -x

set -euo pipefail

LOG_FILE=/tmp/routing.log
exec &> >(tee -a "$LOG_FILE")

while getopts "ih:" OPT; do
    case ${OPT} in
    i)
        INSTALL=true
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))

if test -n "${INSTALL:-}"; then
    sudo mkdir -p /etc/boot.d
    sudo ln -s $(realpath $0) /etc/boot.d
    exit 0
fi

if "$EUID" -eq 0; then
    SUDO_OPT=()
else
    SUDO_OPT=(sudo)
fi

RPI_GUEST=192.168.2.45
RPI_PRIV=192.168.1.238

function iface-with-ip-prefix {
    PREFIX=${1} && shift
    ip a | grep "${PREFIX}" -F -B3 | grep -Po '^[0-9]+: [^:]+: ' |  \
        cut -f2 -d: | tr -d ' '
}

function ip-route-add-ok-if-exists {
    if ! OUTPUT=$(${SUDO_OPT[@]} ip route add ${*} 2>&1); then
        RET=$?
        if ! grep -F "RTNETLINK answers: File exists" <<< "${OUTPUT}"; then
            echo "${OUTPUT}"
            return ${RET}
        else
            echo "warn: skipping file exists error"
        fi
    fi
}

if DEV=$(iface-with-ip-prefix 192.168.2.) && test -n "${DEV:-}" && ping -c3 "${RPI_GUEST}"; then
    ip-route-add-ok-if-exists "${RPI_GUEST}" dev "${DEV}"
    ip-route-add-ok-if-exists 192.168.1.0/24 via "${RPI_GUEST}"
fi

if DEV=$(iface-with-ip-prefix 192.168.1.) && test -n "${DEV:-}" && ping -c3 "${RPI_PRIV}"; then
    ip-route-add-ok-if-exists "${RPI_PRIV}" dev "${DEV}"
    ip-route-add-ok-if-exists 192.168.2.0/24 via "${RPI_PRIV}"
fi

if dig @192.168.1.195 erjoalgo.com; then
    ${SUDO_OPT[@]} insert-text-block -b '# 8704b256-4f22-40ed-a54b-efcdfe8902db-poweredge-resolver' \
         /etc/resolv.conf <<EOF
nameserver 192.168.1.195
EOF
fi
