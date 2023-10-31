#!/bin/bash -x

set -euo pipefail

UMOUNT=false

while getopts "hub:" OPT; do
    case ${OPT} in
    u)
        UMOUNT=true
        ;;
    b)
        BOOT_PARTITION=${OPTARG}
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))

CHROOT=${1} && shift

if test "${UMOUNT:-}" = true; then
    for DIR in ${CHROOT}/{sys,proc,boot,dev/pts,dev}; do
        for _ in $(seq 3); do
            if sudo umount ${DIR}; then
                break
            fi
            echo "umount ${DIR} failed. retrying..."
            sleep 1
        done
    done
    exit 0
fi

test -d "${CHROOT:-}"

sudo mount -t sysfs /sys "${CHROOT}/sys" || true
sudo mount -t proc /proc "${CHROOT}/proc" || true
sudo mount --bind /dev "${CHROOT}/dev" || true
sudo mount -t devpts /dev/pts "${CHROOT}/dev/pts" || true

if test -n "${BOOT_PARTITION:-}"; then
    sudo mount --bind "${BOOT_PARTITION}" "${ROOT}/boot" || true
fi

sudo chroot "${CHROOT}"
