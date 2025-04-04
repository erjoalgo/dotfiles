#!/bin/bash

set -euo pipefail

command -v ustreamer || sudo apt-get install -y ustreamer

DEVICE=/dev/video0
PORT=8080

install-systemd-service.sh ustreamer <<EOF
[Unit]
Description=Start the ustreamer service for octoprint
StartLimitInterval=0

[Service]
ExecStart=$(which ustreamer) --host=0.0.0.0 --port=${PORT} --device=${DEVICE} --io-method=USERPTR --format=MJPEG

Restart=always
RestartSec=5

[Install]
WantedBy=multi-user.target
EOF
