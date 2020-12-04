#!/bin/bash -x

set -euo pipefail

# runtime dependencies
sudo apt install -y ffmpeg libsdl2-2.0-0

# client build dependencies
sudo apt install -y make gcc git pkg-config meson ninja-build \
     libavcodec-dev libavformat-dev libavutil-dev \
     libsdl2-dev

which meson ninja

# server build dependencies
# sudo apt install -y openjdk-11-jdk

cd ${HOME}/git
URL=https://github.com/Genymobile/scrcpy
BASE=$(basename "${URL}")
test -d "${BASE}" || git clone "${URL}"
cd "${BASE}"

meson x --buildtype release --strip -Db_lto=true || true
cd x
ninja

command -v scrcpy
