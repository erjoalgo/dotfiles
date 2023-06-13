#!/bin/bash -x

set -euo pipefail

LSPCI=$(lspci)

function add-non-free-apt-source {
    . /etc/os-release
    if test -n "${VERSION_CODENAME}"; then
        CODENAME=$VERSION_CODENAME
    else
        CODENAME=$(printf '%s\n' "$VERSION" | grep -o '[a-z]*')
    fi

    sudo tee /etc/apt/sources.list.d/non-free.list <<EOF
deb http://deb.debian.org/debian ${CODENAME} non-free
deb-src http://deb.debian.org/debian ${CODENAME} non-free
EOF
    sudo apt-get update
}

function install-wifi-tools {
    sudo apt-get install -y wireless-tools iw wpasupplicant network-manager
    sudo iwconfig
}

INSTALLED=""

if grep -Pi "centrino|Net.*Intel" <<< "$LSPCI" && ! dpkg -s firmware-iwlwifi; then
    add-non-free-apt-source
    sudo apt-get install -u firmware-iwlwifi
    sudo modprobe -r iwlwifi || true
    sudo modprobe iwlwifi
    INSTALLED+=" iwlwifi"
fi

if grep -i "Ethernet.*Broadcom" <<< "${LSPCI}"; then
    add-non-free-apt-source
    sudo apt-get install -uy firmware-bnx2
    INSTALLED+=firmware-bnx2
fi

if grep -Pi "Intel Corporation Cannon Point-LP High Definition Audio Controller" \
        <<< "$LSPCI"; then
    add-non-free-apt-source
    sudo apt-get install -u firmware-sof-signed
    INSTALLED+=" firmware-sof-signed"
fi

if sudo dmesg | grep -i "firmware: failed to load.*rtl"; then
    apt-get install -y firmware-realtek
    modprobe -r realtek
    modprobe realtek
    INSTALLED+=" realtek"
fi

if test -z "${INSTALLED}"; then
    echo ${LSPCI}
    echo "^^ unknown network card!"
    exit ${LINENO}
else
    echo "installed: ${INSTALLED}"
fi
