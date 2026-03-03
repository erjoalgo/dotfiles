#!/bin/bash -x

set -euo pipefail

if test -L ~/.xinitrc; then
    unlink ~/.xinitrc
fi

insert-text-block '# 9604209d-759e-4e6a-be37-770e1ddd39d6-source-xinitrc-profile' \
                  ~/.xinitrc <<EOF
source ${HOME}/git/dotfiles/inits/.xinitrc.profile
EOF
