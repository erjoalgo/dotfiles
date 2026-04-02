#!/bin/bash -x

set -euo pipefail

while getopts "h" OPT; do
    case ${OPT} in
    h)
        less "$0"
        exit 0
        ;;
    *)
        echo "unrecognized flag: ${OPT}" && exit ${LINENO}
        ;;
    esac
done
shift $((OPTIND -1))

which node
MAJOR=$(node -v | grep -Po '(?<=v)[0-9]+')
if test "${MAJOR}" -lt 22; then
    echo "need node 22 or higher for the built-in Websocket client"
    exit ${LINENO}
fi

NODE_DIR=$(dirname $(which node))

install-systemd-service.sh k1c-fan <<EOF
[Unit]
Description=Keep the K1C exhaust fan on

[Service]
ExecStart=${HOME}/git/dotfiles/bin/k1c-fan.js
Restart=always
RestartSec=60
Environment=PATH=${NODE_DIR}:${PATH}
Environment=VERBOSE=false

[Install]
WantedBy=default.target

EOF
