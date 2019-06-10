#!/bin/bash -x

set -euo pipefail


if [ $EUID != 0 ]; then
    sudo "$0" "$@"
    exit $?
fi

function usage  {
    echo "mount-lvm.sh -b <BLOCK> -m <MOUNT_POINT> [-u]"
}

DOMOUNT=true
while getopts "hub:m:" OPT; do
    case ${OPT} in
	u)
	    DOMOUNT=false
	    ;;
        b)
            BLOCK=${OPTARG}
            ;;
        m)
            MOUNT_POINT=${OPTARG}
            ;;
	h)
	    less $0
	    exit 0
	    ;;
    esac
done


if test -z "${MOUNT_POINT:-}" -o -z "${BLOCK:-}"; then
    usage
    exit 1
fi

if test ${DOMOUNT} = true; then
    mkdir -p ${MOUNT_POINT}
    udisksctl unlock -b ${BLOCK}
    vgchange -ay

    select MAPPER_ID in $(dmsetup ls | cut -f1); do
	MAPPER=/dev/mapper/${MAPPER_ID}
        sudo mount ${MAPPER} ${MOUNT_POINT}
        break
    done
else
    MOUNT_POINT=${@:$OPTIND:1}
    BLOCK=$(mount | grep " on ${MOUNT_POINT}" | cut -f1 -d' ') || true
    umount ${MOUNT_POINT} || true
    select MAPPER in $(dmsetup ls | cut -f1); do
        BASE=$(basename ${MAPPER})
        vgchange -an ${BASE} || true
        dmsetup remove ${BASE} || true
    done
    udisksctl lock -p ${BLOCK}
    # udisksctl power-off ${BLOCK}
fi
# lvdisplay -c
