#!/bin/bash -x

cd /tmp/
FFTAR="$(pwd)/firefox.tar.bz2"
FF="firefox"
sudo wget 'https://download.mozilla.org/?product=firefox-latest&os=linux64&lang=en-US' -O "${FFTAR}" || exit ${LINENO}

cd /usr/local/
sudo tar -axvf "${FFTAR}" || exit ${LINENO}
cd "${FF}"

EXE="$(pwd)/firefox/firefox-bin"
sudo update-alternatives --install /usr/local/bin/firefox firefox "${EXE}" 20
sudo update-alternatives --set firefox "${EXE}"
