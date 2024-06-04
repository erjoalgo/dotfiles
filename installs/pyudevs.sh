#!/bin/bash -x

set -euo pipefail

cd "$(realpath $(dirname "${BASH_SOURCE[0]}"))"

python3 -m pip install pyudev

install-systemd-service.sh -u pyudevs <<EOF
[Unit]
Description=Run custom udev scripts via pyudev
Requires=systemd-udevd.service
After=systemd-udevd.service
StartLimitInterval=0

[Service]
ExecStart=$(realpath $(pwd)/../bin/pyudevs.py)
Restart=always
RestartSec=5
Environment=PATH=$PATH:$(pwd)/bin

[Install]
WantedBy=default.target
EOF
