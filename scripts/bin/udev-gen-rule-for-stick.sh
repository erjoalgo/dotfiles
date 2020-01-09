#!/bin/bash -x

set -euo pipefail

while getopts "d:m:o:l:Mh" OPT; do
    case ${OPT} in
    d)
        PARTITION=${OPTARG}
        ;;
    m)
        MOUNT_POINT_PREFIX=${OPTARG}
        ;;
    o)
        MOUNT_OPTS="-o ${OPTARG}"
        ;;
    M)
        NOMOUNT=true
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done

MOUNT_POINT_PREFIX=${MOUNT_POINT_PREFIX:-${HOME}/mnt/}
test -n "${MOUNT_POINT_PREFIX}"

while true; do
    if test -e "${PARTITION:-}"; then
        break
    fi
    sudo blkid
    select PARTITION in $(sudo blkid | cut -f1 -d:); do
        break
    done
done

ID_SERIAL_SHORT=$(udevadm info -n ${PARTITION} |  \
                      grep ID_SERIAL_SHORT |  \
                      cut -d= -f2)

MOUNT_POINT="${MOUNT_POINT_PREFIX}${ID_SERIAL_SHORT}"

mkdir -p ${MOUNT_POINT} || sudo mkdir -p ${MOUNT_POINT}

RULE_FNAME=/etc/udev/rules.d/199-automount-usb-${ID_SERIAL_SHORT}.rules


MOUNT_OPTS=${MOUNT_OPTS:-""}
if test -n "${MOUNT_OPTS}"; then
    MOUNT_OPTS=" ${MOUNT_OPTS}"
fi
if sudo blkid | grep "${PARTITION}.*fat"; then
    MOUNT_OPTS+=" -o umask=000"
fi
cat <<EOF | sudo tee ${RULE_FNAME}
# auto-generated rule to mount device at mount point
ACTION=="add", KERNEL=="sd[a-z][1-9]", SUBSYSTEM=="block", RUN+="$(which mount)${MOUNT_OPTS:-} /dev/%k ${MOUNT_POINT}"
EOF

RELOAD=false

for UDEV_CONFIG in $(find /etc /lib -name systemd-udevd.service -type f 2>/dev/null); do
    if ! grep '^MountFlags=shared' ${UDEV_CONFIG}; then
        echo "warning: need to update ${UDEV_CONFIG} to MontFlags=slave"
        sudo sed -e '/^MountFlags=/d' ${UDEV_CONFIG}
        echo "MountFlags=shared" | sudo tee -a ${UDEV_CONFIG}
        RELOAD=true
    fi
done

sudo udevadm control --reload-rules

if test ${RELOAD} = true; then
    sudo service udev stop
    sudo systemctl daemon-reload
    sudo service udev restart
fi

echo "successfully wrote rule ${RULE_FNAME}, mounting to ${MOUNT_POINT}"

# mount the first time
if test -z "${NOMOUNT:-}" && ! mount | grep -F "${PARTITION}"; then
    sudo mount -o "${MOUNT_OPTS}" "${PARTITION}" "${MOUNT_POINT}"
fi

# Local Variables:
# compile-command: "./write-udevrule-for-stick.sh -d /dev/sda1 -m /mnt/ -o umask=000"
# End:
