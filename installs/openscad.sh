#!/bin/bash -x

set -euo pipefail

URL=https://files.openscad.org/OpenSCAD-2021.01-x86_64.AppImage
EXE=${HOME}/bin/$(basename "${URL}")

if ! test -e "${EXE}"; then
    curl "${URL}" -o "${EXE}"
fi

chmod +x "${EXE}"

sudo apt-get install -y fuse libfuse2
