#!/bin/bash -x

set -euo pipefail

sed -i 's/^exec/ exec/g' ~/.xinitrc

insert-text-block '# b1a3634c-5085-4ace-b25a-472248bd3d54-n64-xinitrc'  \
                  ~/.xinitrc <<EOF
. ${HOME}/.xinitrc.profile
exec mupen64plus /afs/asus.erjoalgo.com/public/n64/roms/current-rom.z64
EOF

