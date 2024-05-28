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

    if test ${VERSION_ID} -ge 12; then
        NON_FREE=non-free-firmware
    else
        NON_FREE=non-free
    fi

    sudo tee /etc/apt/sources.list.d/non-free.list <<EOF
deb http://deb.debian.org/debian ${CODENAME} ${NON_FREE}
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
    sudo apt-get install -y firmware-realtek
    sudo modprobe -r realtek || true
    sudo modprobe realtek
    INSTALLED+=" realtek"
fi

if sudo grep "Possible missing firmware /lib/firmware/radeon" -R /var/log/syslog; then
    sudo apt-get install -y firmware-amd-graphics
fi

if  sudo dmesg | grep 'Direct firmware load for .*failed with error'; then
    sudo apt-get install -y firmware-misc-nonfree
fi

if (sudo grep /var/log/syslog 'idVendor=0bda.*idProduct=b812' ||  \
        sudo lsusb | grep "RTL88x2bu") \
       && ! lsmod | grep 88x2bu; then
    cd ${HOME}/git
    URL=https://github.com/cilynx/rtl88x2bu
    DIR=${HOME}/git/$(basename "${URL}")
    sudo apt-get install -y bc
    test -e "${DIR}" || git clone "${URL}" "${DIR}"
    sudo apt-get install -y linux-headers-$(uname -r)
    pushd .
    cd "${DIR}"
    make
    sudo modprobe cfg80211
    sudo insmod 88x2bu.ko
fi

if test -z "${INSTALLED}"; then
    echo ${LSPCI}
    echo "no drivers found to install"
else
    echo "installed: ${INSTALLED}"
fi
