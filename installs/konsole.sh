#!/bin/bash -x

set -euo pipefail

KONSOLERC=${HOME}/.config/konsolerc

if test -e "$KONSOLERC"; then
    sed -i '/^DefaultProfile=/d' ${KONSOLERC}
fi

insert-text-block '# eab944d5-9973-4f44-b2e0-1b168f164397-konsolerc-defaults'  \
                  ${KONSOLERC} -b << EOF
# konsolerc file tends to be managed by konsole in a site-specific way.
MenuBar=Disabled

[Desktop Entry]
DefaultProfile=erjoalgo.profile

[Favorite Profiles]
Favorites=erjoalgo.profile

[KonsoleWindow]
ShowMenuBarByDefault=false
EOF


xmlstarlet ed -d \
           '//gui/ToolBar'  \
           "${HOME}.local/share/kxmlgui5/konsole/sessionui.rc"
