#!/bin/bash -x

set -euo pipefail

OVPN=${1} && shift
BASE=$(basename "${OVPN}" .ovpn)

sudo cp "${OVPN}" "/etc/openvpn/${BASE}.conf"

SERVICE=openvpn@${BASE}

sudo systemctl enable ${SERVICE}.service

echo "use service ${SERVICE} start|stop"
