#!/bin/bash -x

set -euo pipefail

while getopts "s:b:h" OPT; do
    case ${OPT} in
    s)
        SERVER_ADDRESS=${OPTARG}
        ;;
    b)
        DEVICE_BIND_PATH=${OPTARG}
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))

sudo apt-get install -y usbip
sudo modprobe vhci-hcd

sudo usbip attach -r "${SERVER_ADDRESS}" -b "${DEVICE_BIND_PATH}"

