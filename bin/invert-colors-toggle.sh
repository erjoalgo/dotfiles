#!/bin/bash -x

set -euo pipefail


if pgrep compton; then
    pkill -9 compton
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
    compton --invert-color-include "${ARG}"
fi
