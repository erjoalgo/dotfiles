#!/bin/bash -x

set -euo pipefail

UMOUNT=false

while getopts "huv:ab" OPT; do
    case ${OPT} in
    u)
        UMOUNT=true
        ;;
    b)
        LUKS_PARTITION=${OPTARG}
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))

if test -z "${LUKS_PARTITION:-}"; then
    echo "select partition to mount: "
    OLDIFS=$IFS
    IFS=$'\n'
    sudo blkid
    select LUKS_PARTITION in  \
        $(sudo dmsetup ls | cut -f1); do
        break
    done
fi

MNT=/mnts/$(basename ${LUKS_PARTITION})

if test "${UMOUNT:-}" = true; then
    sudo umount "${ROOT}" || true
    sudo vgchange -an || true
    sudo cryptsetup luksClose "${PARTITION}"
    exit 0
else
    sudo cryptsetup luksOpen ${LUKS_PARTITION} "${PARTITION_NAME}"
    sudo vgchange -ay
fi


sudo mkdir -p ${MNT}

