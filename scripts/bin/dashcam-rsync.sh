#!/bin/bash -x

set -euo pipefail

DEST=${HOME}/Videos/$(date +"%d-%m-%Y")
DCIM=/mnt/sdcard/DCIM

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

rsync --remove-source-files -arv "${DCIM}" "${DEST}/"

# TODO persist on cloud storage
