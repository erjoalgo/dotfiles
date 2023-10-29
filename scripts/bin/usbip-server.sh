#!/bin/bash -x

set -euo pipefail

FILTER_OPT=(cat)
while getopts "b:h" OPT; do
    case ${OPT} in
    b)
        # bind device
        DEVICE=${OPTARG}
        ;;
    f)
        FILTER_OPT=(grep -B1 "${OPTARG}")
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

if test -z "${DEVICE:-}"; then
	sudo usbip list -l
	CHOICES=$(sudo usbip list -l | "${FILTER_OPT[@]}" | grep -Po '(?<=^ - busid )[^ ]+')
	if test 1 -eq $(wc -l <<< "${CHOICES}"); then
	  DEVICE=${CHOICES}
	else
	  echo "select device to bind: " 1>&2
	  select DEVICE in $CHOICES; do
	    break
	  done
	fi
fi

sudo usbip bind -b "${DEVICE}"

sudo usbipd

