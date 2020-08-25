#!/bin/bash -x

set -euo pipefail

SERVICE_NAME=${1} && shift

# example service
cat <<EOF > /dev/null
[Unit]
Description=Auto Reverse SSH
# Requires=systemd-networkd-wait-online.service
# After=systemd-networkd-wait-online.service

[Service]
# ExecStart=/usr/bin/autossh -f rpi
ExecStart=/usr/bin/autossh -M 0 -o "ExitOnForwardFailure=yes" -o "ServerAliveInterval 30" -o "ServerAliveCountMax 3" -NR \${REMOTE_SSH_PORT}:127.0.0.1:\${LOCAL_SSH_PORT} \${SSH_USER_HOST} -i /home/ealfonso/.ssh/id_rsa -p4410

[Install]
WantedBy=multi-user.target
EOF

sudo insert-text-block "# YBhmitLFbimkywNqu0jXW996vavpPrtP-${SERVICE_NAME}"  \
     /etc/systemd/system/${SERVICE_NAME}.service < /dev/stdin

sudo systemctl enable ${SERVICE_NAME}.service
sudo systemctl daemon-reload
sudo service ${SERVICE_NAME} start
