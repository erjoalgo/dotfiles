#!/bin/bash -x

set -euo pipefail

if ! command -v picom; then
    sudo apt-get install -y picom
fi

if pgrep -af picom | grep -v defunct; then
    pkill -9 picom
    emacsclient-wrapper.sh -e '(redshift-unload-dark-theme)' &
else
    emacsclient-wrapper.sh -e '(redshift-load-dark-theme)' &
    picom --config ~/git/dotfiles/inits/picom.conf &
    disown
fi
