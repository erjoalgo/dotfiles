#!/bin/bash -x

set -euo pipefail

sudo apt install -y build-essential cmake pkg-config curl git \
     clang ninja-build ccache \
    libgtk-3-dev \
    libcanberra-gtk3-module \
    libgl-dev \
    libasound2-dev \
    libao-dev \
    libopenal-dev \
    libsdl3-dev \
    libpulse-dev \
    libudev-dev

URL=https://github.com/ares-emulator/ares/wiki/Build-Instructions-For-Linux
REPO=${HOME}/git/ares

test -d "${REPO}" || git clone "${URL}" "${REPO}"

cd "${REPO}"

# rm -rf ./build
mkdir -p build && cd build
cmake .. -G Ninja

cmake --build .

# cmake --install . --prefix "${HOME}/bin"
cmake --install . --prefix "${HOME}"
