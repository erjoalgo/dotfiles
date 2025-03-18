#!/bin/bash -x

set -euo pipefail

URL=https://github.com/Quedale/OnvifDeviceManager.git
REPO=${HOME}/git/$(basename "${URL}" .git)
if ! test -d "${REPO}"; then
    git clone "${URL}" "${REPO}"
fi

if false; then
    sudo apt install -y git pkg-config libgtk-3-dev make g++ \
         python3-pip bison flex libtool libssl-dev zlib1g-dev \
         libasound2-dev libgudev-1.0-dev libx11-xcb-dev gettext \
         libpulse-dev nasm libntlm0-dev

    pip install meson
    pip install ninja
    pip install cmake
fi

cd ${REPO}
./autogen.sh --prefix=$(pwd)/dist --enable-latest
make -j$(nproc)

make deb
sudo dpkg -i onvifmgr_0.0.deb
