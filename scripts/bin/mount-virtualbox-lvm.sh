#!/bin/bash -x

set -euo pipefail

UMOUNT=false

while getopts "huv:" OPT; do
    case ${OPT} in
    u)
        UMOUNT=true
        ;;
    v)
        VDI=${OPTARG}
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))

if test -z "${VDI:-}"; then
    echo "select VDI: "
    OLDIFS=$IFS
    IFS=$'\n'
    select VDI in $(ls -1 ~/VirtualBox\ VMs/*/*vdi); do
        break
    done
fi

VM_NAME=$(basename "${VDI}" .vdi)

MNT=/mnts/${VM_NAME}
RAW=${MNT}-raw
ROOT=${MNT}-root
BOOT=${MNT}-boot
# LOOP_DEVICE="/dev/loop_$(ls -1 /dev/loop* | wc -l)"
LOOP_DEVICE="/dev/loop8"
PARTITION_NAME="vm-${VM_NAME}"

if test "${UMOUNT:-}" = true; then
    for DIR in ${ROOT}/{sys,proc,boot,dev/pts,}; do
        sudo umount ${DIR} || true
    done
    sudo umount "${BOOT}" || true
    sudo umount "${ROOT}" || true
    sudo vgchange -an || true
    sudo cryptsetup luksClose "${PARTITION_NAME}"
    sudo losetup -d "${LOOP_DEVICE}"
    sudo umount "${RAW}"
    exit 0
fi


sudo mkdir -p ${RAW} ${ROOT} ${BOOT}

function mount_exists {
    MOUNT="${1}" && shift
    mount | grep -F "on ${MOUNT}"
}

function list-raw-partitions {
    sudo find ${RAW} -maxdepth 1 -type f -name 'vol*'
}

if ! mount_exists "${RAW}"; then
    sudo vboximg-mount -i "${VDI}" "${RAW}"
fi

if ! mount_exists "${ROOT}"; then

    if ! sudo dmsetup info "${PARTITION_NAME}"; then
        list-raw-partitions | xargs sudo du --apparent-size -h
        echo "select encrypted LUKS partition: "
        select LUKS_PARTITION in $(list-raw-partitions); do
            break
        done

        sudo losetup -P "${LOOP_DEVICE}" "${LUKS_PARTITION}" || true
        sudo cryptsetup luksOpen "${LOOP_DEVICE}" "${PARTITION_NAME}"
        sudo vgchange -ay
    fi

    VG=$(sudo pvdisplay /dev/mapper/${PARTITION_NAME} -C --noheadings |  \
             tr -s ' '| cut -f3 -d' ')
    echo "select root partition: "
    select ROOTP in $(ls /dev/${VG}); do
        break
    done

    sudo mount "/dev/${VG}/${ROOTP}" "${ROOT}"
    sudo ls "${ROOT}"
fi

if ! mount_exists "${BOOT}"; then
    list-raw-partitions | xargs sudo du --apparent-size -h
    echo "select boot partition: "
    select BOOTP in $(sudo find ${RAW} -maxdepth 1 -type f); do
        break
    done

    sudo mount ${BOOTP} ${BOOT}
    sudo ls "${BOOT}"
fi

sudo mount --bind "${BOOT}" "${ROOT}/boot" || true
sudo mount -t sysfs /sys "${ROOT}/sys" || true
sudo mount -t proc /proc "${ROOT}/proc" || true
sudo mount -t devpts /dev/pts "${ROOT}/dev/pts" || true

echo "now sudo chroot ${ROOT}"
