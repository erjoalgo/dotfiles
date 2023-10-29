#!/bin/bash -x

set -euo pipefail

FILTER_OPT=(cat)
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

test -n "${REMOTE_SERVER_ADDRESS}"

CHOICES=$(usbip list -r "${REMOTE_SERVER_ADDRESS}" |  \
              "${FILTER_OPT[@]}" | \
              cut -f1 -d: | tr -d ' ')
if test 1 -eq $(wc -l <<< "${CHOICES}"); then
    DEVICE="${CHOICES}"
else
    usbip list -r "${REMOTE_SERVER_ADDRESS}"
    echo "select device attach to" 1>&2
    select DEVICE in "${CHOICES}"; do
        break
    done
fi

sudo usbip attach -r "${REMOTE_SERVER_ADDRESS}" -b "${DEVICE}"

