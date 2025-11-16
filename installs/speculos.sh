#!/bin/bash -x

set -euo pipefail

sudo apt install -y \
    cmake gcc-arm-linux-gnueabihf libc6-dev-armhf-cross gdb-multiarch \
    python3-pyqt5 python3-construct python3-flask-restful python3-jsonschema \
    python3-mnemonic python3-pil python3-pyelftools python3-requests \
    qemu-user-static libvncserver-dev

URL=https://github.com/LedgerHQ/speculos/
REPO=${HOME}/git/$(basename "${URL}")
test -d "${REPO}" || git clone "${URL}" "${REPO}"
cd "${REPO}"
git pull --ff-only


cmake -B build/ -DCMAKE_BUILD_TYPE=Debug -DWITH_VNC=1 -S .

pip3 install .
