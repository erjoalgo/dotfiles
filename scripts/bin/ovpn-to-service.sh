#!/bin/bash -x

set -euo pipefail

OVPN=${1} && shift
BASE=OVPN
BASE=$(basename "${BASE}" .ovpn)
BASE=$(basename "${BASE}" .conf)

sudo mv "${OVPN}" "/etc/openvpn/${BASE}.conf"

SERVICE=openvpn@${BASE}

sudo systemctl enable ${SERVICE}.service

echo "use service ${SERVICE} start|stop"
