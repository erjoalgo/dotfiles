#!/bin/bash -x

set -euo pipefail

while getopts "d:ha:" OPT; do
    case ${OPT} in
    d)
        DEVICE=${OPTARG}
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))

test -n "${DEVICE:-}"
DEVICE=/dev/sdb

FIRMWARE=$(echo ${HOME}/git/Marlin/.pio/build/*/firmware*bin)
test -e "${FIRMWARE}"

function get-mount-point {
    DEVICE=${1} && shift
    mount | grep -Po "(?<=${DEVICE} on )[^ ]*"
}

if ! MOUNTP=$(get-mount-point "${DEVICE}"); then
    mount-partition.sh -b "${DEVICE}"
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
