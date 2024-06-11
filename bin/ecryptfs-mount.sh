#!/bin/bash -x

set -euo pipefail
while getopts "ha:" OPT; do
    case ${OPT} in
    h)
        less "$0"
        exit 0
        ;;
    *)
        echo "unrecognized flag: ${OPT}" && exit ${LINENO}
        ;;
    esac
done
shift $((OPTIND -1))

THIS_CELL=$(cat /etc/openafs/ThisCell)
SRC="/afs/${THIS_CELL}/public/backup"
DEST="${HOME}/mnts/backup"

test -e "${SRC}"
mkdir -p "${DEST}"

sudo mount -t ecryptfs  \
     -o ecryptfs_cipher=aes,ecryptfs_key_bytes=32,ecryptfs_passthrough=n,ecryptfs_enable_filename_crypto=y,key=passphrase \
     "${SRC}" "${DEST}"
