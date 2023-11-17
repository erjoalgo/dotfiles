#!/bin/bash -x

set -euo pipefail

# BIN_D=${BIN_D:-/usr/local/bin}
BIN_D=${BIN_D:-${HOME}/bin}

mkdir -p ${BIN_D} && cd ${BIN_D}

SCRIPT=openvpn-install.sh
if ! command -v ${SCRIPT} && ! test -e ${SCRIPT}; then
    curl -O https://raw.githubusercontent.com/angristan/openvpn-install/master/openvpn-install.sh
    chmod +x ${SCRIPT}
fi

sudo ./${SCRIPT}
