#!/bin/bash -x

set -euo pipefail

DEST=${HOME}/Videos/$(date +"%d-%m-%Y")
DCIM=/mnts/sdcard/DCIM

while getopts "hd:s:" OPT; do
    case ${OPT} in
        s)
            # source directory
            DCIM=${OPTARG}
            ;;
        d)
            # output directory
            DEST=${OPTARG}
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

# sudo chown -R $(whoami) "${DCIM}"

rsync --remove-source-files -arv "${DCIM}" "${DEST}/"

sudo find "${DCIM}" -depth -mindepth 1 -type d -exec rmdir {} \;

# TODO persist on cloud storage
