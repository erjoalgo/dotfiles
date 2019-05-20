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

MOUNT_OPTS=""
if sudo blkid | grep "${DEVNAME}.*fat"; then
    MOUNT_OPTS="-o umask=000"
fi

udev-gen-rule-for-stick.sh -d ${DEVNAME}  -m ${HOME}/mnt -l ${SYMLINK}

PARTITION=${DEVNAME}

if ! mount | grep "^${PARTITION}"; then
    sudo mount -o umask=000 ${PARTITION} ${SYMLINK}
fi

for SECRET in \
    .password-store \
    .gnupg; do

    DEST="${HOME}/${SECRET}"
    SRC="${SYMLINK}/nosync/${SECRET}"

    if test -d "${DEST}" -a ! -L "${DEST}"; then
        CMD="mv ${DEST} ${DEST}.bak"
        read -p "${CMD}: "
        ${CMD}
    fi

    if ! test -d "${SRC}"; then
        echo "warning: missing secret at ${SRC}"
    else
        ln -sfT "${SRC}" "${DEST}"
    fi
done
