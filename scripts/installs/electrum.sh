#!/bin/bash -x

set -euo pipefail

sudo apt-get install -y python3-pyqt5 libsecp256k1-0 python3-cryptography

if ! command -v electrum; then
    cd /tmp
    wget https://download.electrum.org/4.0.3/Electrum-4.0.3.tar.gz
    python3 -m pip install --user Electrum-4.0.3.tar.gz
fi
