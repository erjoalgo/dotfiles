#!/bin/bash -x

set -euo pipefail

cd $(dirname "${BASH_SOURCE[0]}")

touch ~/.xinitrc

sed -i 's/^exec/ exec/g' ~/.xinitrc

insert-text-block '# b1a3634c-5085-4ace-b25a-472248bd3d54-n64-xinitrc'  \
                  ~/.xinitrc <<EOF
. ${HOME}/.xinitrc.profile
exec mupen64plus /afs/asus.erjoalgo.com/public/n64/roms/current-rom.z64
EOF

./autologin-on-tty.sh

if systemctl list-units --full -all | grep lightdm.service; then
    sudo systemctl disable lightdm.service
    sudo systemctl stop lightdm.service
fi
