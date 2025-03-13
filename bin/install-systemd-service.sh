#!/bin/bash -x

set -euo pipefail

while getopts "huo" OPT; do
    case ${OPT} in
    u)
        USER_SERVICE=true
        ;;
    o)
        OVERRIDE=true
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))

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

if test -n "${USER_SERVICE:-}"; then
    CONF=${HOME}/.config/systemd/user/${SERVICE_NAME}.service
    mkdir -p $(dirname "${CONF}")
    insert-text-block \
        "# 7119bd41-07d2-429a-ae51-cc16f0878169-${SERVICE_NAME}"  \
        "${CONF}" < /dev/stdin
    systemctl --user enable ${SERVICE_NAME}.service
    systemctl --user daemon-reload
    systemctl --user start ${SERVICE_NAME}.service
else
    FILENAME=/etc/systemd/system/${SERVICE_NAME}.service
    if test -n "${OVERRIDE:-}"; then
        FILENAME=/etc/systemd/system/${SERVICE_NAME}.service.d/override.conf
        sudo mkdir -p $(dirname "${FILENAME}")
    fi
    sudo insert-text-block "# YBhmitLFbimkywNqu0jXW996vavpPrtP-${SERVICE_NAME}"  \
          "${FILENAME}" < /dev/stdin
    sudo systemctl enable ${SERVICE_NAME}.service
    sudo systemctl daemon-reload
    sudo service ${SERVICE_NAME} start
fi
