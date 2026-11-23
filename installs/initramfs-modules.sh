#!/bin/bash

set -euo pipefail

# Choose which kernel modules go into each initramfs, then rebuild them:
#
# dep:  only the drivers this machine uses. About a third of the size
#       (51 MB vs 139 MB on phantom), so a small /boot fits the three
#       kernels apt holds during an upgrade. But the disk won't boot on
#       other hardware until the initramfs is rebuilt there: from a live
#       USB, chroot in, delete the drop-in below and run
#       update-initramfs -u -k all.
# most: nearly every driver (Debian's default): the disk boots on other
#       hardware as is, but three kernels' worth need about 500 MB of
#       /boot, more than phantom's 456 MB.
#
# usage: initramfs-modules.sh [dep|most]   (prompts without an argument)

DROPIN=/etc/initramfs-tools/conf.d/modules-dep
CURRENT=most
if test -e "${DROPIN}"; then
    CURRENT=dep
fi

CHOICE=${1:-}
if test -z "${CHOICE}"; then
    echo "initramfs modules: dep = this machine's drivers only (small, but"
    echo "won't boot on other hardware until rebuilt); most = nearly all"
    echo "drivers (Debian's default; portable, but larger: compare below)."
    ls -la /boot/initrd.img-*
    df -h /boot
    read -r -p "dep or most? [${CURRENT}] " CHOICE
    CHOICE=${CHOICE:-${CURRENT}}
fi

case ${CHOICE} in
    dep)
        # conf.d is read after initramfs.conf, so this overrides it.
        echo MODULES=dep | sudo tee "${DROPIN}" >/dev/null
        ;;
    most)
        sudo rm -f "${DROPIN}"
        ;;
    *)
        echo "expected dep or most, got: ${CHOICE}" >&2
        exit 1
        ;;
esac

set -x
sudo update-initramfs -u -k all
ls -la /boot/initrd.img-*
df -h /boot
