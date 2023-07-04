#!/bin/bash -x

set -euo pipefail

UMOUNT=false

while getopts "huv:a" OPT; do
    case ${OPT} in
    u)
        UMOUNT=true
        ;;
    v)
        VDI=${OPTARG}
        ;;
    a)
        AUTO=true
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

function list-raw-partitions-by-size {
    sudo du -ba "${RAW}" | grep vol | sort -n | cut -f2
}

if ! mount_exists "${RAW}"; then
    sudo vboximg-mount -i "${VDI}" "${RAW}"
fi

if ! mount_exists "${ROOT}"; then

    if ! sudo dmsetup info "${PARTITION_NAME}"; then
        list-raw-partitions-by-size | xargs sudo du --apparent-size -h
        echo "select encrypted LUKS partition: "
        if test -n "${AUTO:-}"; then
            # take the largest partition
            LUKS_PARTITION=$(list-raw-partitions-by-size | tail -1)
        else
            select LUKS_PARTITION in $(list-raw-partitions-by-size); do
                break
            done
        fi

        sudo losetup -P "${LOOP_DEVICE}" "${LUKS_PARTITION}" || true
        sudo cryptsetup luksOpen "${LOOP_DEVICE}" "${PARTITION_NAME}"
        sudo vgchange -ay
    fi

    VG=$(sudo pvdisplay /dev/mapper/${PARTITION_NAME} -C --noheadings |  \
             tr -s ' '| cut -f3 -d' ')
    if test -n "${AUTO:-}"; then
        ROOTP=root
    else
        echo "select root partition: "
        select ROOTP in $(ls /dev/${VG}); do
            break
        done
    fi
    sudo mount "/dev/${VG}/${ROOTP}" "${ROOT}"
    sudo ls "${ROOT}"
fi

if ! mount_exists "${BOOT}"; then
    list-raw-partitions-by-size | xargs sudo du --apparent-size -h
    echo "select boot partition: "
    if test -n "${AUTO:-}"; then
        # take the smallest partition
        BOOTP=$(list-raw-partitions-by-size | head -1)
    else
        select BOOTP in $(sudo find ${RAW} -maxdepth 1 -type f); do
            break
        done
    fi

    sudo mount ${BOOTP} ${BOOT}
    sudo ls "${BOOT}"
fi

sudo mount --bind "${BOOT}" "${ROOT}/boot" || true
sudo mount -t sysfs /sys "${ROOT}/sys" || true
sudo mount -t proc /proc "${ROOT}/proc" || true
sudo mount -t devpts /dev/pts "${ROOT}/dev/pts" || true

echo "now sudo chroot ${ROOT}"
