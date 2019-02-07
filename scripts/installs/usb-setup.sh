#!/bin/bash -x

set -euo pipefail

while getopts "hd:" OPT; do
    case ${OPT} in
    d)
        DEVNAME=${OPTARG}
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done

if test -z "${DEVNAME:-}"; then
    sudo blkid
    read -p "insert usb disk..."
    sudo blkid
    read -p "enter partition block path: " DEVNAME
    test -e "${DEVNAME}"
fi



SYMLINK=${HOME}/.usb-drive-symlink
udev-gen-rule-for-stick.sh -d ${DEVNAME} -o umask=000  \
                           -m ${HOME}/mnt \
                           -l ${SYMLINK}


# ln -s ${MNT}
