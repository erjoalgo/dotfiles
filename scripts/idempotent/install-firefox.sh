#!/bin/bash -x

cd /tmp/
FFTAR="firefox.tar.bz2"
FF=$(basename "${FFTAR}" .tar.bz2)

if ! test -f ${FFTAR}; then
    sudo wget 'https://download.mozilla.org/?product=firefox-latest&os=linux64&lang=en-US' -O "${FFTAR}" || exit ${LINENO}
fi

INSTALL_DIR=/usr/local/${FF}

if ! test -d ${INSTALL_DIR}; then
    sudo tar -C -axvf "${FFTAR}" || exit ${LINENO}
fi

cd  || exit ${LINENO}


EXE="${INSTALL_DIR}/firefox-bin"
test -f ${EXE} || exit ${LINENO}

sudo update-alternatives --install /usr/local/bin/firefox firefox "${EXE}" 20
sudo update-alternatives --set firefox "${EXE}"
