#!/bin/bash -x

set -euo pipefail

for CAND in ${HOME}/bin/OpenSCAD*AppImage  \
                   ${HOME}/Downloads/OpenSCAD*AppImage \
                   $(which openscad); do
    if test -e "${CAND}"; then
        EXE="${CAND}"
        break
    fi
done

test -x "${EXE}"


QT_SCALE_FACTOR=2 "${EXE}" "$@"
