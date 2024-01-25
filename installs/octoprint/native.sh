#!/bin/bash -x

set -euo pipefail

sudo apt-get install -y authbind

PORT_80=/etc/authbind/byport/80
sudo touch "${PORT_80}"
sudo chmod 777 "${PORT_80}"

# allow access to /dev/ttyUSB*
sudo usermod -a -G dialout $(whoami)

pip install octoprint

install-systemd-service.sh octoprint <<EOF
[Unit]
Description=octoprint-native
After=default.target

[Service]
ExecStart=authbind --deepep $(which octoprint) serve --port 80 --debug
User=$(whoami)
SyslogIdentifier=octoprint-native
Restart=always
Type=simple
RestartSec=30s

[Install]
WantedBy=default.target
EOF
