#!/bin/bash -x

set -euo pipefail

while getopts "d:n:h" OPT; do
    case ${OPT} in
    d)
        RAW_DRIVE=${OPTARG}
        ;;
    n)
        # optional
        NAME=${OPTARG}
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))


test -n "${RAW_DRIVE:-}"


TOP=/var/vmdk

sudo mkdir -p "${TOP}"

NAME=${NAME:-$(basename "${RAW_DRIVE}")}.vmdk
VMDK=${TOP}/${NAME}

echo "attempting to close existing medium first if it exists..."
sudo vboxmanage closemedium "${VMDK}" --delete || true

sudo vboxmanage createmedium disk --filename="${VMDK}" \
     --variant=RawDisk --format=VMDK --property RawDrive="${RAW_DRIVE}"

sudo chown ${USER} "${VMDK}" "${RAW_DRIVE}"
