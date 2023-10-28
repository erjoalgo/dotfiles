#!/bin/bash -x

set -euo pipefail

while getopts "b:h" OPT; do
    case ${OPT} in
    b)
        BIND_DEVICE=${OPTARG}
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))

sudo apt-get install -y usbip
sudo modprobe usbip_host

# TODO auto-select bind device
sudo usbip list -l
echo "select device to bind: " 1>&2
select DEVICE in $(sudo usbip list -l | grep -B1 'serial converter' | grep -Po '(?<=^ - busid )[^ ]+'); do
    break
done

sudo usbip bind -b "${DEVICE}"

sudo usbipd

