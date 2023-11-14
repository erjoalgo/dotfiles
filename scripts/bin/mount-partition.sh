#!/bin/bash -x

set -euo pipefail

UMOUNT=false


while getopts "hub:" OPT; do
    case ${OPT} in
    u)
        UMOUNT=true
        ;;
    b)
        PARTITION=${OPTARG}
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))

if test -z "${PARTITION:-}"; then
    echo "select partition to mount: "
    OLDIFS=$IFS
    IFS=$'\n'
    select PARTITION in  \
        $(sudo lsblk -o PATH,NAME,MODEL,SIZE,FSTYPE,MOUNTPOINT |  \
              grep --color=always -E '.*crypto_LUKS|$'); do
        PARTITION=$(cut -f1 -d' ' <<< "${PARTITION}")
        PARTITION=$(sed  -r "s/\x1B\[([0-9]{1,3}(;[0-9]{1,2};?)?)?[mGK]//g"  \
                             <<< "${PARTITION}")
        break
    done
    tput init # reset the colors!
    tput sgr0
    IFS=${OLDIFS}
fi

function get-partition-attr {
    KEY_SPEC=${1} && shift
    COLUMN=${1} && shift
    KEY_COL=$(cut -f1 -d= <<< "${KEY_SPEC}")
    KEY_VAL=$(cut -f2 -d= <<< "${KEY_SPEC}")
    sudo lsblk -o ${KEY_COL},${COLUMN} | grep "${KEY_VAL}" | tr -s ' ' | cut -f2 -d' '
}

FSTYPE=$(get-partition-attr PATH="${PARTITION}" FSTYPE)

if test "${FSTYPE}" = crypto_LUKS; then
else
    DEVICE_PATH="${PARTITION}"
fi



function get-mountpoint {
    LABEL=$(get-partition-attr PATH="${DEVICE_PATH}" LABEL)
    echo /mnts/${LABEL:-$(basename "${DEVICE_PATH}")}
}

if test "${UMOUNT:-}" = true; then
    MNT=$(get-mountpoint)
    sudo umount "${MNT}" || true
    sudo vgchange -an || true
    sudo cryptsetup luksClose "${DEVICE_PATH}"
    exit 0
else

    LUKS_NAME=$(basename ${PARTITION})
    if test "${FSTYPE}" = crypto_LUKS && ! sudo dmsetup info ${LUKS_NAME}; then
        sudo cryptsetup luksOpen "${PARTITION}" "${LUKS_NAME}"
        sudo vgchange -ay
    fi
    MNT=$(get-mountpoint)
    sudo mkdir -p ${MNT}
    if ! sudo mount  "${DEVICE_PATH}" "${MNT}"; then
        VG=$(sudo pvdisplay "${DEVICE_PATH}" -C --noheadings |  \
                 tr -s ' '| cut -f3 -d' ')
        echo "select root partition: "
        select ROOTP in $(ls /dev/${VG}); do
            break
        done
        sudo mount  "/dev/${VG}/${ROOTP}" "${MNT}"
    fi
    sudo ls "${MNT}"
fi
