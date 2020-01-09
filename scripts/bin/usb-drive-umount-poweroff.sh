#!/bin/bash -x

set -euo pipefail

MNT=${HOME}/.usb-drive-symlink

while getopts "m:h" OPT; do
    case ${OPT} in
    m)
        MOUNT_POINT=${OPTARG}
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))

MOUNT_POINT=${MOUNT_POINT:-}
PARTITION=${PARTITION:-}

if test -n "${MOUNT_POINT}"; then
    for PART in $(findmnt -n -o SOURCE --target ${MOUNT_POINT}); do
        if test -e ${PART}; then
            PARTITION=${PART}
            break
        fi
    done
else
    select PARTITION in $(sudo blkid | cut -f1 -d:); do
        break
    done
fi

test -e "${PARTITION}"
BLOCK_DEVICE="/dev/$(lsblk -no pkname ${PARTITION})"

if test -z "${MOUNT_POINT}"; then
    MOUNT_POINT=$(sudo mount | grep -Po "(?<=${PARTITION} on )[^ ]+") || true
fi

if test -e "${MOUNT_POINT}"; then
    sudo umount ${MOUNT_POINT}
fi

sudo udisksctl power-off --block-device ${BLOCK_DEVICE}
