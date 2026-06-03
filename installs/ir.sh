#!/bin/bash -x

set -euo pipefail

cd "$(realpath $(dirname "${BASH_SOURCE[0]}"))"

install-systemd-service.sh ir <<EOF
[Unit]
Description=Run the IR/RF http service

[Service]
ExecStart=$(which irremote)  --port 2727
Restart=always
RestartSec=5
Environment=PATH=$PATH:$(realpath $(pwd)/../bin)

[Install]
WantedBy=multi-user.target
EOF
