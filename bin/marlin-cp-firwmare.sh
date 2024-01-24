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

FIRMWARE=$(find ${HOME}/git/Marlin/.pio/build/STM32F103RE_creality -name firmware* \
               | head -1)
test -e "${FIRMWARE}"

function get-mount-point {
    DEVICE=${1} && shift
    mount | grep -Po "(?<=${DEVICE} on )[^ ]*"
}
if ! MOUNTP=$(get-mount-point "${DEVICE}"); then
    mount-partition.sh -b "${DEVICE}"
    MOUNTP=$(get-mount-point "${DEVICE}")
fi


if ls ${MOUNTP}/*firmware*bin; then
    if diff "${FIRMWARE}" ${MOUNTP}/*firmware*bin; then
        read -p"warn: new and old firmware files are identical!"
    fi
fi

sudo rm -f ${MOUNTP}/*bin
sudo cp "${FIRMWARE}" ${MOUNTP}/Ender-3S1_Pro_HWv$(head -c2  <<< "${RANDOM}")S1_301_SWV2.0.8.23F1_F103_FDM_LASER.bin
sudo ls "${MOUNTP}"

umount-poweroff.sh -qm "${MOUNTP}"
