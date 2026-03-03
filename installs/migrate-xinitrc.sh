#!/bin/bash -x

set -euo pipefail

XINITRC=${HOME}/.xinirc
XINITRC_PROFILE=${HOME}/git/dotfiles/inits/.xinitrc.profile

if test -L "${XINITRC}"; then
    unlink "${XINITRC}"
fi

insert-text-block '# 9604209d-759e-4e6a-be37-770e1ddd39d6-source-xinitrc-profile' \
                  "${XINITRC}" <<EOF
source "${XINITRC_PROFILE}"
EOF
