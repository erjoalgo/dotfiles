#!/bin/bash -x

set -euo pipefail

while getopts "c:s:b:h" OPT; do
    case ${OPT} in
    c)
        COMMAND_LINE=${OPTARG}
        ;;
    s)
        SCHEME=${OPTARG}
        ;;
    b)
        BLOCK_ID=${OPTARG}
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))

APP_NAME=$(grep -Po '^[^ ]+' <<< "${COMMAND_LINE}")

insert-text-block "# ${BLOCK_ID}" \
                  ${HOME}/.local/share/applications/mimeapps.list \
                  <<EOF
[Added Associations]
x-scheme-handler/mailto=${APP_NAME}.desktop
EOF

DESKTOP_ENTRY=${HOME}/.local/share/applications/${APP_NAME}.desktop

insert-text-block "# ${BLOCK_ID}" ${DESKTOP_ENTRY} <<EOF
[Desktop Entry]
Exec=${COMMAND_LINE}
Terminal=false
Type=Application
MimeType=x-scheme-handler/${SCHEME}
EOF

sudo ln -sf ${DESKTOP_ENTRY} /usr/share/applications

xdg-mime default $(basename ${DESKTOP_ENTRY}) x-scheme-handler/${SCHEME}
xdg-mime query default x-scheme-handler/${SCHEME} | grep -F "${APP_NAME}"
