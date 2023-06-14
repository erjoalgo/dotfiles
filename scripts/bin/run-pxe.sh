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

DHCPD=$(which run-dhcpd.sh)
ATFTPD=$(which run-atftpd.sh)

sudo ${DHCPD} -i "${IFACE}" -x "${PXE_FILENAME}" &

sudo ${ATFTPD} -d "${DIRECTORY}"

wait $(jobs -p)
