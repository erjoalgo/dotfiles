#!/bin/bash -x

set -euo pipefail


INI=/usr/share/games/mupen64plus/InputAutoCfg.ini
ORIG="${INI}.orig"
if ! test -e "${ORIG}"; then
    sudo cp "${INI}" "${ORIG}"
fi

OWN="${HOME}/git/dotfiles/inits/InputAutoCfg.ini"

sudo ln -sf "${OWN}" "${INI}"

sudo chgrp input $(which mupen64plus)
sudo chmod g+s $(which mupen64plus)

sudo insert-text-block '; f62dcec4-1169-4ba2-b47f-6048170a50ad-apply-all-possible-subscren-delay-fixes' \
     /usr/share/games/mupen64plus/mupen64plus.ini <<EOF
[57A9719AD547C516342E1A15D5C28C3D]
GoodName=Legend of Zelda, The - Ocarina of Time (U) (V1.2) [!]
CRC=693BA2AE B7F14E9F
Players=1
SaveType=SRAM
Rumble=Yes
; Subscreen Delay Fixes
Cheat0=801DAE8B 0002
EOF

DEST=${HOME}/.local/share/mupen64plus
if test -d "${DEST}" -a ! -L "${DEST}"; then
    mv -n "${DEST}" "${DEST}.bak"
fi
ln -fs /afs/asus.erjoalgo.com/public/n64/mupen64plus ~/.local/share/
