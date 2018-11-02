#!/bin/bash -x

set -euo pipefail


if [ $EUID != 0 ]; then
    sudo "$0" "$@"
    exit $?
fi

DOMOUNT=true
while getopts "hu" OPT; do
    case ${OPT} in
	u)
	    DOMOUNT=false
	    ;;
	h)
	    less $0
	    exit 0
	    ;;
    esac
done


if test ${DOMOUNT} = true; then
    BLOCK=${1} && shift
    MNT=${1} && shift

    mkdir -p ${MNT}

    udisksctl unlock -b ${BLOCK}
    vgchange -ay

    select MAPPER_ID in $(dmsetup ls | cut -f1); do
	MAPPER=/dev/mapper/${MAPPER_ID}
        sudo mount ${MAPPER} ${MNT}
        break
    done
else
    MNT=${@:$OPTIND:1}
    BLOCK=$(mount | grep " on ${MNT}" | cut -f1 -d' ') || true
    umount ${MNT} || true
    select MAPPER in $(dmsetup ls | cut -f1); do
        BASE=$(basename ${MAPPER})
        vgchange -an ${BASE} || true
        dmsetup remove ${BASE} || true
    done
    udisksctl lock -p ${BLOCK}
    # udisksctl power-off ${BLOCK}
fi
# lvdisplay -c
