#!/bin/bash -x

set -euo pipefail

function usage  {
    echo "backup-machine.sh -d OUTPUT_DIRECTORY"
}

while getopts "hd:" OPT; do
    case ${OPT} in
    d)
        OUTPUT_DIRECTORY=${OPTARG}
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))

if test -z "${OUTPUT_DIRECTORY}"; then
    usage
    exit $LINENO
fi

mkdir -p ${OUTPUT_DIRECTORY}

cd ${OUTPUT_DIRECTORY}

sudo dpkg --get-selections > installed-software.txt
zip -9r var.zip /var/
zip -9r etc.zip /etc

sudo find / | gzip > directory-structure.gz
sudo mount > mount.txt
sudo fdisk > fdisk.txt

if command -v pgdump; then
    pg_dump dbname > pg_dump.sql
fi
