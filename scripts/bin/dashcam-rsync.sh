#!/bin/bash -x

set -euo pipefail

DEST=${HOME}/Videos/$(date +"%d-%m-%Y")
DCIM=/mnts/sdcard/DCIM

while getopts "hds:" OPT; do
    case ${OPT} in
        d)
            # output directory
            DEST=${OPTARG}
            ;;
        s)
            # source directory
            DCIM=${OPTARG}
            ;;
        h)
            less $0
            exit 0
            ;;
    esac
done
shift $((OPTIND -1))

if ! test -d ${DCIM}; then
    echo "DCIM directory does not exist: ${DCIM}"
fi

sudo rsync --remove-source-files -arv "${DCIM}" "${DEST}/"

sudo find "${DCIM}" -depth -mindepth 1 -type d -exec rmdir {} \;
# TODO persist on cloud storage
