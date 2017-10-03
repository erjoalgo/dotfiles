#!/bin/bash -x

set -euo pipefail


# 'https://download.mozilla.org/?product=firefox-latest&os=linux64&lang=en-US'
# URL="https://download.mozilla.org/?product=firefox-52.1.1esr-SSL&os=linux64&lang=en-US"
URL="https://download.mozilla.org/?product=firefox-45.9.0esr-SSL&os=linux64&lang=en-ZA"
cd /tmp/
FFTAR="firefox.tar.bz2"
FF=$(basename "${FFTAR}" .tar.bz2)

if test -f ${FFTAR}; then
    rm -f ${FFTAR} || exit ${LINENO}
    true
fi

wget "${URL}" -O "${FFTAR}" || exit ${LINENO}

INSTALL_DIR=/usr/local/${FF}

if test -d ${INSTALL_DIR}; then
    test -n "${FF}" || exit ${LINENO}
    sudo rm -rf /usr/local/${FF}
fi

sudo tar -C /usr/local -axvf "${FFTAR}" || exit ${LINENO}
cd  || exit ${LINENO}


EXE="${INSTALL_DIR}/firefox-bin"
test -f ${EXE} || exit ${LINENO}

sudo update-alternatives --install /usr/local/bin/firefox firefox "${EXE}" 20
sudo update-alternatives --set firefox "${EXE}"
