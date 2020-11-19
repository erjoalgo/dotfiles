#!/bin/bash -x

set -euo pipefail

sudo apt-get install -y python3-pyqt5 libsecp256k1-0 python3-cryptography \
     python3-setuptools python3-pip python3-pyqt5

if ! command -v electrum || test -n "${FORCE:-}"; then
    SRC=${HOME}/src
    mkdir -p "${SRC}"
    URL=https://download.electrum.org/4.0.4/Electrum-4.0.4.tar.gz
    cd "${SRC}"
    test -e $(basename "${URL}") || wget "${URL}"
    # gpg --verify Electrum-4.0.4.tar.gz.asc
    python3 -m pip install --user Electrum-4.0.4.tar.gz
fi
python3 -m pip install pycryptodomex

wget -q -O - \
     https://raw.githubusercontent.com/LedgerHQ/udev-rules/master/add_udev_rules.sh  \
    | sudo bash

RULES_FILE=/etc/udev/rules.d/20-hw1.rules
if ! grep OWNER "${RULES_FILE}"; then
    sudo sed -i "s/\"$/\", OWNER=\"${USER}\"/g" "${RULES_FILE}"
fi
