#!/bin/bash -x

set -euo pipefail

SWAP=/swap.file

while getopts "hf:g:" OPT; do
    case ${OPT} in
        f)
            SWAP=${OPTARG}
            ;;
        g)
            GB=${OPTARG}
            ;;
        h)
            less $0
            exit 0
            ;;
    esac
done

test 0 = ${UID}

test ! -e ${SWAP}

CNT=$(expr 1024 \* ${GB})
BS=1024k

dd if=/dev/zero of=${SWAP} bs=${BS} count=${CNT}
mkswap -f ${SWAP}
chmod 0600 ${SWAP}
echo "${SWAP} none swap sw 0 0 " >> /etc/fstab
swapon ${SWAP}
