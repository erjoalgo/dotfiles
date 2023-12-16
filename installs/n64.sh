#!/bin/bash -x

set -euo pipefail

sudo apt-get install -y mupen64plus-qt

INI=/usr/share/games/mupen64plus/InputAutoCfg.ini
ORIG="${INI}.orig"
if ! test -e "${ORIG}"; then
    sudo cp "${INI}" "${ORIG}"
fi

OWN="${HOME}/git/dotfiles/inits/InputAutoCfg.ini"

sudo ln -sf "${OWN}" "${INI}"

sudo chgrp input $(which mupen64plus)
sudo chmod g+s $(which mupen64plus)
