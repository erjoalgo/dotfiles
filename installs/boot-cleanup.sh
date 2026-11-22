#!/bin/bash

set -euo pipefail

# Recover from a full /boot: purge every versioned kernel except the
# running and the newest one (with their headers), switch to small
# MODULES=dep initramfs images, and finish the upgrade that ran out of
# space. Asks before purging; -n only prints the plan.

DRY_RUN=false
while getopts "nh" OPT; do
    case ${OPT} in
        n)
            DRY_RUN=true
            ;;
        h)
            less "$0"
            exit 0
            ;;
        *)
            exit 1
            ;;
    esac
done

cd "$(dirname "$(realpath "$0")")"

# Installed images, including half-configured ones (not config-files-only).
VERSIONS=$(dpkg-query -W -f='${db:Status-Abbrev} ${Package}\n' 'linux-image-[0-9]*' |
               grep -Po '^.[^nc]. linux-image-\K\S+(?<!-dbg)$' |
               sort -V)
RUNNING=$(uname -r)
NEWEST=$(tail -1 <<< "${VERSIONS}")

PURGE=()
for VERSION in ${VERSIONS}; do
    if test "${VERSION}" = "${RUNNING}" -o "${VERSION}" = "${NEWEST}"; then
        continue
    fi
    PURGE+=("linux-image-${VERSION}")
    if dpkg-query -W -f='${db:Status-Abbrev}' "linux-headers-${VERSION}" 2>/dev/null |
            grep -q '^.[^nc]'; then
        PURGE+=("linux-headers-${VERSION}")
    fi
done

echo "running: ${RUNNING}, newest: ${NEWEST}"
echo "to purge: ${PURGE[*]:-nothing}"
df -h /boot
if ${DRY_RUN}; then
    exit 0
fi

if test ${#PURGE[@]} -gt 0; then
    read -r -p "purge these? [y/N] " ANSWER
    test "${ANSWER}" = y
    # dpkg rather than apt: apt would first retry configuring the packages
    # that failed for lack of space, and fail again.
    sudo dpkg --purge "${PURGE[@]}"
fi

./initramfs-modules-dep.sh
sudo dpkg --configure -a
ls -la /boot/initrd.img-*
df -h /boot
