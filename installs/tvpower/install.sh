#!/bin/bash -x

set -euo pipefail

cd "$(realpath $(dirname "${BASH_SOURCE[0]}"))"

CONF=/etc/sysctl.d/99-sysrq.conf
LINE=kernel.sysrq=1
if ! grep -F "${LINE}" "${CONF}"; then
    sudo insert-text-block \
         '# db18ac07-26c5-45bc-ba08-59ade8a402fb-enable-sysrq'  \
         "${CONF}" <<EOF
${LINE}
EOF
    sudo sysctl -p "${CONF}"
fi


# Build with DKMS, which rebuilds the module for each new kernel.
NAME=tvpower
VERSION=$(grep -Po '^PACKAGE_VERSION="\K[^"]+' dkms.conf)
SRC=/usr/src/${NAME}-${VERSION}
KVER=$(uname -r)

# The flavor metapackage (e.g. linux-headers-amd64) pulls in headers for
# every new kernel, which DKMS needs to build against it.
sudo apt-get install -y dkms "linux-headers-${KVER##*-}"
sudo apt-get install -y "linux-headers-${KVER}" ||
    echo "no headers for running kernel ${KVER}; reboot into the newest one"

if lsmod | grep "${NAME}"; then
    sudo modprobe -r "${NAME}" || true
fi

# Earlier versions of this script copied the module into each kernel's
# tree by hand and hooked kernel installs with a symlink to itself.
sudo rm -f /lib/modules/*/kernel/drivers/${NAME}.ko \
     /etc/kernel/postinst.d/install.sh

# Re-add the source so changes to it get rebuilt.
if test -n "$(dkms status -m "${NAME}" -v "${VERSION}")"; then
    sudo dkms remove "${NAME}/${VERSION}" --all
fi
sudo rm -rf "${SRC}"
sudo mkdir -p "${SRC}"
sudo cp dkms.conf Makefile tvpower.c "${SRC}"
sudo dkms add "${NAME}/${VERSION}"
for KERNEL in $(ls /lib/modules); do
    if test -d "/lib/modules/${KERNEL}/build"; then
        sudo dkms install "${NAME}/${VERSION}" -k "${KERNEL}"
    fi
done

sudo modprobe "${NAME}"

sudo $(which install-systemd-service.sh) dmesg-listener <<EOF
[Unit]
Description=dmesg listener

[Service]
User=ealfonso
ExecStart=$(pwd)/dmesg-listener.sh
Restart=always
RestartSec=60

[Install]
WantedBy=multi-user.target
EOF

sudo tee -a /proc/sysrq-trigger <<< 'x'

sudo tee /etc/modules-load.d/tvpower.conf <<< tvpower
