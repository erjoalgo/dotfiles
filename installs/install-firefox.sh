#!/bin/bash -x

set -euo pipefail

command -v firefox && exit 0 || true

# 'https://download.mozilla.org/?product=firefox-latest&os=linux64&lang=en-US'
# URL="https://download.mozilla.org/?product=firefox-52.1.1esr-SSL&os=linux64&lang=en-US"
URL="https://download.mozilla.org/?product=firefox-45.9.0esr-SSL&os=linux64&lang=en-ZA"
cd /tmp/
FFTAR="firefox.tar.bz2"
FF=$(basename "${FFTAR}" .tar.bz2)
test -n "${FF}"

test -f ${FFTAR} && rm -f ${FFTAR}

wget "${URL}" -O "${FFTAR}"

INSTALL_DIR=/usr/local/${FF}

test -d ${INSTALL_DIR} && sudo rm -rf /usr/local/${FF}

sudo tar -C /usr/local -axvf "${FFTAR}"
cd


EXE="${INSTALL_DIR}/firefox-bin"
test -f ${EXE}

sudo update-alternatives --install /usr/local/bin/firefox firefox "${EXE}" 20
sudo update-alternatives --set firefox "${EXE}"
