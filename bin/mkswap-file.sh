#!/bin/bash -x

set -euo pipefail

SWAP=${SWAP:-/swap.file}

FREEMEM_KB=$(grep -Po '(?<=MemTotal:) *[0-9]+(?= kB)' /proc/meminfo | tr -d ' ')
FREEMEM_GB=$(bc <<< "scale=2; ${FREEMEM_KB} / 1024^2")
GB=${GB:-$(printf "%.0f" $(bc <<< "scale=0;(${FREEMEM_GB} + 2)"))}

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

shift $((OPTIND -1))
if test $# -gt 0; then
    less $0
    echo "unexpected positional args"
    exit ${LINENO}
fi

test ! -e ${SWAP}

function mkswap {
    SWAP=${1} && shift
    GB=${1} && shift
    CNT=$(expr 1024 \* ${GB})
    BS=1024k

    dd if=/dev/zero of=${SWAP} bs=${BS} count=${CNT}
    chmod 0600 ${SWAP}
    mkswap -f ${SWAP}
    echo "${SWAP} none swap sw 0 0 " >> /etc/fstab
    swapon ${SWAP}
}

mkswap "${SWAP}" "${GB}"
