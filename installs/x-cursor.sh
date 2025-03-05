#!/bin/bash -x

set -euo pipefail

CURSOR_THEME=${CURSOR_THEME:-noche-diamante}

DEST="/usr/share/icons/${CURSOR_THEME}"

if ! test -d "${DEST}"; then
    git submodule init
    git submodule update
    git submodule sync
    SRC=$(realpath ../submodules/${CURSOR_THEME})
    test -d "${SRC}"
    sudo cp -r "${SRC}" "${DEST}"
fi

sudo sed -i "s/^\(Inherits=\).*/\1${CURSOR_THEME}/" \
     /usr/share/icons/default/index.theme
