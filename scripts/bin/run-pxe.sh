#!/bin/bash -x

set -euo pipefail

while getopts "hi:d:x:" OPT; do
    case ${OPT} in
    i)
        IFACE=${OPTARG}
        ;;
    d)
        DIRECTORY=${OPTARG}
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

DHCPD_CMD=$(which run-dhcpd.sh)
ATFTPD_CMD=$(which run-atftpd.sh)
ATFTPD_CMD+=" -d${DIRECTORY}"


if test -n "${IFACE:-}"; then
    DHCPD_CMD+=" -i${IFACE}"
fi
if test -n "${PXE_FILENAME:-}"; then
    DHCPD_CMD+=" -x${PXE_FILENAME}"
fi

PXE_FILENAME=${PXE_FILENAME:-}

( sudo ${DHCPD_CMD} || true; kill $$) &

( sudo ${ATFTPD_CMD} || true; kill $$) &

wait $(jobs -p)
