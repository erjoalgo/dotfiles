#!/bin/bash -x

set -euo pipefail

FILTER_OPT=()
while getopts "r:f:h" OPT; do
    case ${OPT} in
    r)
        REMOTE_SERVER_ADDRESS=${OPTARG}
        ;;
    f)
        FILTER_OPT=(grep "${OPT}")
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

usbip list -r "${REMOTE_SERVER_ADDRESS}"

echo "select device attach to" 1>&2
select DEVICE in $(usbip list -r "${REMOTE_SERVER_ADDRESS}" |  \
                       "${FILTER_OPT[@]}" | \
                           cut -f1 -d: | tr -d ' '); do
    break
done

sudo usbip attach -r "${REMOTE_SERVER_ADDRESS}" -b "${DEVICE}"

