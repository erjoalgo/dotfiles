#!/bin/bash -x

set -euo pipefail

while getopts "b:h" OPT; do
    case ${OPT} in
    b)
        BIND_DEVICE=${OPTARG}
        ;;
    f)
        FILTER_OPT="grep -B1 '${OPTARG}' | "
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

sudo usbip list -l
CHOICES=$(sudo usbip list -l | ${FILTER_OPT} grep -Po '(?<=^ - busid )[^ ]+')

if test -eq 1 $(wc -l <<< "${CHOICES}"); then
  DEVICE=${CHOICES}
else
  echo "select device to bind: " 1>&2
  select DEVICE in $CHOICES; do
    break
  done
fi

sudo usbip bind -b "${DEVICE}"

sudo usbipd

