#!/bin/bash -x

set -euo pipefail

sudo blkid

read -p "insert usb disk..."

sudo blkid

read -p "enter partition block path: " DEVNAME

udev-gen-rule-for-stick.sh -d ${DEVNAME} -o umask= -m ${HOME}/mnt

MNT=${HOME}/.usb-drive-symlink

# ln -s ${MNT}
