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


MODULE=tvpower.ko
MODULE_BASENAME=$(basename "${MODULE}" .ko)
if lsmod | grep "${MODULE_BASENAME}"; then
    sudo rmmod "${MODULE}" || true
    sudo modprobe -r "${MODULE_BASENAME}" || true
fi


make clean
make

sudo insmod "${MODULE}"

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

sudo tee -a /proc/sysrq-trigger <<< 'P'
