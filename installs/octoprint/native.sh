#!/bin/bash -x

set -euo pipefail

sudo apt-get install -y authbind

PORT_80=/etc/authbind/byport/80
sudo touch "${PORT_80}"
sudo chmod 777 "${PORT_80}"

# allow access to /dev/ttyUSB*
sudo usermod -a -G dialout $(whoami)

sudo apt-get install -y libffi-dev

# try to prevent error: AttributeError: cython_sources [duplicate]
pip install "cython<3.0.0" wheel
pip install "pyyaml==5.4.1" --no-build-isolation
pip install -U setuptools
pip install legacy-cgi

pip install octoprint

install-systemd-service.sh -u octoprint <<EOF
[Unit]
Description=octoprint-native
After=default.target

[Service]
ExecStart=authbind --deep $(which octoprint) serve --port 80
SyslogIdentifier=octoprint-native
Restart=always
Type=simple

[Install]
WantedBy=default.target
EOF

sudo ufw allow 80/tcp

sudo loginctl enable-linger
sudo loginctl enable-linger ${USER}
