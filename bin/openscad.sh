#!/bin/bash -x

set -euo pipefail

EXE=$(which ${HOME}/bin/OpenSCAD*AppImage  \
            ${HOME}/Downloads/OpenSCAD*AppImage \
            openscad | sed "1!d")

command -v "${EXE}"

QT_SCALE_FACTOR=3 "${EXE}" "$@"
