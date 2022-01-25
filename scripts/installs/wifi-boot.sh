#!/bin/bash -x

set -euo pipefail

LSPCI=$(lspci)

function add_non-free_apt_source {
    sudo tee /etc/apt/sources.list.d/non-free.list <<EOF
deb http://deb.debian.org/debian buster non-free
deb-src http://deb.debian.org/debian buster non-free
EOF
    sudo apt-get update
}

if grep -i centrino <<< "$LSPCI"; then
    add_non-free_apt_source
    sudo apt-get install -u firmware-iwlwifi
    sudo modprobe -r iwlwifi || true
    sudo modprobe iwlwifi
elif grep -i "Ethernet.*Broadcom" <<< "${LSPCI}"; then
    add_non-free_apt_source
    sudo apt-get install -uy firmware-bnx2
else
    echo "unknown network card: ${LSPCI}"
    exit $LSPCI
fi
sudo apt-get install -y wireless-tools iw wpasupplicant

sudo iwconfig

# apt-get install -y firmware-realtek
# modprobe -r realtek
# modprobe realtek
# 109  modprobe -r realtek ; modprobe realtek
