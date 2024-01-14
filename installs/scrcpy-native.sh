#!/bin/bash -x

set -euo pipefail

# for Debian/Ubuntu

sudo apt install -y ffmpeg libsdl2-2.0-0 wget \
                 gcc git pkg-config meson ninja-build libsdl2-dev \
                 libavcodec-dev libavdevice-dev libavformat-dev libavutil-dev \
                 libswresample-dev libusb-1.0-0 libusb-1.0-0-dev

URL=https://github.com/Genymobile/scrcpy
REPO=${HOME}/git/$(basename "${URL}")
test -d "${REPO}" || git clone "${URL}" "${REPO}"
cd "${REPO}"

./install_release.sh
