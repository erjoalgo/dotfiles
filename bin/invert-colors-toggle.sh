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
            TelegramDesktop \
            qimgv \
        ; do
        if test -n "${ARG:-}"; then
            ARG+=" || "
        fi
        ARG+="class_g=\"${CLASS}\""
    done
    # 'class_g="Chromium" || class_g="Zathura" || class_g="matplotlib"'
    picom --invert-color-include "${ARG}"
fi
