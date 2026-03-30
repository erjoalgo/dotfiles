#!/bin/bash -x

set -euo pipefail

sudo apt-get install -y libgstreamer1.0-0  \
     libgstreamer-plugins-base1.0-0 \
     libwebkit2gtk-4.1-dev

URL=https://github.com/CrealityOfficial/CrealityPrint/releases/download/v7.0.1/CrealityPrint_Ubuntu2404-V7.0.1.4212-x86_64-Release.AppImage

LOCAL=${HOME}/bin/$(basename "${URL}")

if ! test -e "${LOCAL}"; then
    curl "${URL}" -o "${LOCAL}"
fi

chmod +x "${LOCAL}"
