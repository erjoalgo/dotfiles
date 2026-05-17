#!/bin/bash -x

set -euo pipefail

if ! command -v picom; then
    sudo apt-get install -y picom
fi

if pgrep -af picom | grep -v defunct; then
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
            Gsmartcontrol \
            firefox-esr \
            Pavucontrol \
            pavucontrol \
            Creality \
            Wireshark \
            'Tor Browser' \
            Evince \
            PrusaSlicer \
            Cura \
            cura \
            Com.github.xournalpp.xournalpp \
        ; do
        if test -n "${ARG:-}"; then
            ARG+=" || "
        fi
        ARG+="class_g=\"${CLASS}\""
    done
    # 'class_g="Chromium" || class_g="Zathura" || class_g="matplotlib"'
    picom --invert-color-include "${ARG}"
fi
