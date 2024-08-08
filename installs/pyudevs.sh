#!/bin/bash -x

set -euo pipefail

cd "$(realpath $(dirname "${BASH_SOURCE[0]}"))"

python3 -m pip install pyudev

sudo apt-get install -y python3-pyudev

install-systemd-service.sh -u pyudevs <<EOF
[Unit]
Description=Run custom udev scripts via pyudev
StartLimitInterval=0

[Service]
ExecStart=$(realpath $(pwd)/../bin/pyudevs.py)
Restart=always
RestartSec=60
Environment=PATH=$PATH:$(pwd)/bin

[Install]
WantedBy=default.target
EOF
