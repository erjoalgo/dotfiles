#!/bin/bash -x

set -euo pipefail

while getopts "m:qh" OPT; do
    case ${OPT} in
    m)
        MOUNT_POINT=${OPTARG}
        ;;
    q)
        set +x # quiet
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))

PARTITION=${PARTITION:-}

if test -n "${MOUNT_POINT:-}"; then
    for PART in $(findmnt -n -o SOURCE --target "${MOUNT_POINT:-}"); do
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

PKNAME=$(lsblk -no pkname ${PARTITION})
if test -n "${PKNAME}"; then
    BLOCK_DEVICE="/dev/${PKNAME}"
else
    BLOCK_DEVICE=$(lsblk -no PATH ${PARTITION})

fi

for MOUNT_POINT in \
    $(sudo mount | grep -Po "${BLOCK_DEVICE}[0-9]* on [^ ]+" |  \
          cut -d' ' -f3 | tr '\n' ' '); do
    sudo umount ${MOUNT_POINT}
done

if ! command -v udisksctl; then
    sudo apt-get install -y udisks2
fi

sudo udisksctl power-off --block-device ${BLOCK_DEVICE}
