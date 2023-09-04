#!/bin/bash -x

set -euo pipefail

while getopts "d:v:h" OPT; do
    case ${OPT} in
    d)
        RAW_DRIVE=${OPTARG}
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))


test -n "${RAW_DRIVE:-}"


TOP=/etc/vmdk

sudo mkdir -p "${TOP}"

VMDK=${TOP}/$(basename "${RAW_DRIVE}").vmdk

echo "attempting to close existing medium first if it exists..."
sudo vboxmanage closemedium "${VMDK}" --delete || true

sudo vboxmanage createmedium disk --filename="${VMDK}" \
     --variant=RawDisk --format=VMDK --property RawDrive="${RAW_DRIVE}"

sudo chown ${USER} "${VMDK}" "${RAW_DRIVE}"
