#!/bin/bash -x

set -euo pipefail

cd "$(realpath $(dirname "${BASH_SOURCE[0]}"))"

install-systemd-service.sh sedation <<EOF
[Unit]
Description=Run user-provided sedation command based on idle time, lid and battery state
StartLimitInterval=0

[Service]
ExecStart=$(realpath $(pwd)/sedation.py)
Restart=always
RestartSec=5
Environment=PATH=$PATH:$(realpath $(pwd)/../bin)

[Install]
WantedBy=multi-user.target
EOF
