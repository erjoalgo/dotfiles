#!/bin/bash -x

set -euo pipefail


if pgrep compton; then
    pkill -9 compton
else
    compton --invert-color-include 'class_g="Chromium" || class_g="Zathura"'
fi
