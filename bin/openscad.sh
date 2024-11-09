#!/bin/bash -x

set -euo pipefail

EXE=$(which ${HOME}/Downloads/OpenSCAD-2024.08.05.ai20224-x86_64.AppImage  \
            openscad | sed "1!d")
command -v "${EXE}"

QT_SCALE_FACTOR=3 "${EXE}" "$@"
