#!/bin/bash -x

set -euo pipefail

MOUNT_PARTITION_CMD=(mount-partition.sh -o rw,umask=000)
DISK_UUID=${DISK_UUID:-52CC-A374}

while getopts "d:k:ha:" OPT; do
    case ${OPT} in
    k)
        DISK_UUID=${OPTARG}
        ;;
    d)
        DEVICE=${OPTARG}
        MOUNT_PARTITION_CMD+=("-b" "${DEVICE}")
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))

if test -n "${DISK_UUID:-}" -a -e /dev/disk/by-uuid/${DISK_UUID}; then
    MOUNT_PARTITION_CMD+=("-k" "${DISK_UUID}")
fi

function get-partition-by-attr {
    KEY_SPEC=${1} && shift
    COLUMN=${1} && shift
    KEY_COL=$(cut -f1 -d= <<< "${KEY_SPEC}")
    KEY_VAL=$(cut -f2 -d= <<< "${KEY_SPEC}")
    sudo lsblk -o ${KEY_COL},${COLUMN} | grep "${KEY_VAL}" | tr -s ' ' | cut -f2 -d' '
}

if test -n "${DISK_UUID:-}"; then
    DEVICE=$(get-partition-by-attr UUID=${DISK_UUID} PATH)
fi

test -n "${DEVICE:-}"

FIRMWARE=$(echo ${HOME}/git/Marlin/.pio/build/*/firmware*bin)
test -e "${FIRMWARE}"

function get-mount-point {
    DEVICE=${1} && shift
    mount | grep -Po "(?<=${DEVICE} on )[^ ]*"
}

if ! MOUNTP=$(get-mount-point "${DEVICE}"); then
    ${MOUNT_PARTITION_CMD[@]}
    MOUNTP=$(get-mount-point "${DEVICE}")
fi

if ls ${MOUNTP}/*.bin; then
    if diff "${FIRMWARE}" ${MOUNTP}/*.bin; then
        read -p"warn: new and old firmware files are identical!"
    fi
fi

sudo rm -f ${MOUNTP}/*bin
BASENAME="GD-Ender-3 ProHW4.2.2SW2.0.8.2CRTouchFilamentEuropeMulti"
BASENAME="${RANDOM}-${BASENAME}"
cp "${FIRMWARE}" "${MOUNTP}/${BASENAME}.bin"
sudo ls -l "${MOUNTP}"

umount-poweroff.sh -q "${MOUNTP}"
