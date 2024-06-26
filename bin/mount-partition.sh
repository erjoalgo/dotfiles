#!/bin/bash -x

set -euo pipefail

UMOUNT=false


OPTS_OPT=()
while getopts "hub:k:o:w" OPT; do
    case ${OPT} in
    u)
        UMOUNT=true
        ;;
    b)
        PARTITION=${OPTARG}
        ;;
    k)
        UUID=${OPTARG}
        ;;
    o)
        OPTS_OPT=(-o ${OPTARG})
        ;;
    w)
        OPTS_OPT=(-o rw,umask=000)
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))

function get-partition-by-attr {
    KEY_SPEC=${1} && shift
    COLUMN=${1} && shift
    KEY_COL=$(cut -f1 -d= <<< "${KEY_SPEC}")
    KEY_VAL=$(cut -f2 -d= <<< "${KEY_SPEC}")
    sudo lsblk -o ${KEY_COL},${COLUMN} | grep "${KEY_VAL}" | tr -s ' ' | cut -f2 -d' '
}

if test -n "${UUID:-}"; then
    PARTITION=$(get-partition-by-attr UUID=${UUID} PATH)
fi

if test -z "${PARTITION:-}"; then
    echo "select partition to mount: "
    OLDIFS=$IFS
    IFS=$'\n'
    select PARTITION in  \
        $(sudo lsblk -o PATH,NAME,MODEL,SIZE,FSTYPE,MOUNTPOINT,UUID |  \
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

FSTYPE=$(get-partition-by-attr PATH="${PARTITION}" FSTYPE)

if test "${FSTYPE}" = crypto_LUKS; then
    LUKS_NAME="decrypted-${UID}"
    DEVICE_PATH="/dev/mapper/${LUKS_NAME}"
    MAPPER=/dev/mapper/${LUKS_NAME}
else
    DEVICE_PATH="${PARTITION}"
fi



function get-unique-label {
    LABEL=$(get-partition-by-attr PATH="${DEVICE_PATH}" LABEL)
    PART_UID=${LABEL:-$(get-partition-by-attr PATH="${DEVICE_PATH}" UUID)}
    echo "${PART_UID}"
}

function get-mountpoint {
    PART_UID=$(get-unique-label)
    echo /mnts/${PART_UID:-$(basename "${DEVICE_PATH}")}
}

function get-volume-for-mapper {
    # TODO more reliable way of mapping volume groups to partitions
    # assumes VG is contained in one physical partition
    MAPPER=${1} && shift
    sudo lvdisplay --maps |  \
        grep -P 'Physical volume|VG Name' \
        | grep -B1 "${MAPPER}" |  \
        grep -Po '(?<=VG Name).*' |  \
        tr -d ' ' | head -1
}

if test "${UMOUNT:-}" = true; then
    MNT=$(get-mountpoint)
    sudo umount "${MNT}" || true
    if ! sudo vgchange -an $(get-volume-for-mapper); then
        sudo vgchange -an || true
    fi
    sudo cryptsetup luksClose "${MAPPER}"
    exit 0
else
    PART_UID=$(get-unique-label)
    if test "${FSTYPE}" = crypto_LUKS && ! sudo dmsetup info ${LUKS_NAME}; then
        sudo cryptsetup luksOpen "${PARTITION}" "${LUKS_NAME}"
        if ! sudo vgchange -ay $(get-volume-for-mapper); then
            sudo vgchange -ay || true
        fi
    fi
    MNT=$(get-mountpoint)
    sudo mkdir -p ${MNT}
    if ! sudo mount  "${DEVICE_PATH}" "${MNT}" ${OPTS_OPT[@]}; then
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
