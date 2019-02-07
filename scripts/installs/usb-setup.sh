#!/bin/bash -x

set -euo pipefail

sudo blkid

read -p "insert usb disk..."

sudo blkid

read -p "enter partition block path: " DEVNAME

SYMLINK=${HOME}/.usb-drive-symlink
udev-gen-rule-for-stick.sh -d ${DEVNAME} -o umask=000  \
                           -m ${HOME}/mnt \
                           -l ${SYMLINK}


# ln -s ${MNT}
