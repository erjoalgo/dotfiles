#!/bin/bash -x

set -euo pipefail

MNT=${HOME}/.usb-drive-symlink

# REALPATH=$(python -c "import os; print(os.path.realpath('${MNT}'))")
# MOUNT_LINE=$(sudo mount | grep -F "${REALPATH}")
# PARTITION=$(grep -o "^[^ 	]*" <<< "${MOUNT_LINE}")

PARTITION=${1:-} && shift || true

if test -z "${PARTITION}"; then
    for PART in $(findmnt -n -o SOURCE --target ${MNT}); do
        if test -e ${PART}; then
            PARTITION=${PART}
            break
        fi
    done
fi

if test -z "${PARTITION}"; then
    sudo blkid
    read -p"could not locate partition, enter it manually: " \
         PARTITION
fi

BLOCK_DEVICE="/dev/$(lsblk -no pkname ${PARTITION})"

if test -e "${MNT}"; then
    sudo umount ${MNT}
fi

sudo udisksctl power-off --block-device  ${BLOCK_DEVICE}
