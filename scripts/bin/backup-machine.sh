#!/bin/bash -x

set -euo pipefail

function usage  {
    echo "usage: backup-machine.sh -d OUTPUT_DIRECTORY"
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

if test -z "${OUTPUT_DIRECTORY:-}"; then
    usage
    exit $LINENO
fi

mkdir -p "${OUTPUT_DIRECTORY}"

cd "${OUTPUT_DIRECTORY}"

for DIR in /var/ /etc; do
    OUT=$(tr / ! <<< "${DIR}").tar.gz
    test -s ${OUT} || sudo tar -czf ${OUT} "${DIR}"
done

sudo apt-get install -y sysv-rc-conf
for CMD in \
    "uname -r" \
        "uname -m" \
        "lsb_release -a" \
        "sysv-rc-conf --list" \
        "mount" \
        "fdisk --list" \
        "find /" \
        "dpkg --get-selections" \
    ; do
    OUT="$(sed 's|[ /]|-|g' <<< ${CMD}).txt.gz"
    test -s "${OUT}" || sudo ${CMD} | gzip > "${OUT}"
done

if command -v pg_dump; then
    OUT=pg_dump.sql.gz; test -s ${OUT} ||  \
        sudo -upostgres pg_dumpall | gzip > ${OUT}
fi

du -h *
