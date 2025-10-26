#!/bin/bash -x

set -euo pipefail

if ! command -v picom; then
    sudo apt-get install -y picom
fi

if pgrep picom; then
    pkill -9 picom
else
    ARG=""
    for CLASS in  \
        Chromium \
            matplotlib  \
            OpenSCAD  \
            Zathura \
            Xournal \
        ; do
        if test -n "${ARG:-}"; then
            ARG+=" || "
        fi
        ARG+="class_g=\"${CLASS}\""
    done
    picom --invert-color-include "${ARG}"
fi
