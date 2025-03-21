#!/bin/bash -x

set -euo pipefail

while getopts "hi:" OPT; do
    case ${OPT} in
    i)
        IFACE=${OPTARG}
        ;;
    h)
        less "$0"
        exit 0
        ;;
    *)
        echo "unrecognized flag: ${OPT}" && exit ${LINENO}
        ;;
    esac
done
shift $((OPTIND -1))

CONF=/etc/NetworkManager/NetworkManager.conf

function all-ifaces {
    find /sys/class/net -mindepth 1 -maxdepth 1 -exec basename {} \;
}

if test -z "${IFACE:-}"; then
    echo "select interface to unmanage from NetworkManager: " 1>&2
    select IFACE in $(all-ifaces); do
        break
    done
fi

function iface-mac {
    IFACE=${1} && shift
    cat /sys/class/net/${IFACE}/address
}

MAC=$(iface-mac "${IFACE}")

sudo insert-text-block  \
    "# 61e7cb70-4191-4afb-98d1-6feda443e280-unmanage-${IFACE}" \
    "${CONF}" <<EOF
[keyfile]
unmanaged-devices=mac:${MAC}
EOF
