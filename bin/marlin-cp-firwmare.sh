#!/bin/bash -x

set -euo pipefail

MOUNT_PARTITION_CMD=(mount-partition.sh)
while getopts "d:k:ha:" OPT; do
    case ${OPT} in
    d)
        DEVICE=${OPTARG}
        MOUNT_PARTITION_CMD+=("-b" "${DEVICE}")
        ;;
    k)
        DISK_UUID=${OPTARG}
        MOUNT_PARTITION_CMD+=("-k" "${DISK_UUID}")
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
sudo cp "${FIRMWARE}" "${MOUNTP}/${BASENAME}.bin"
sudo ls "${MOUNTP}"

umount-poweroff.sh -qm "${MOUNTP}"
