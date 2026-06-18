#!/bin/bash -x

set -euo pipefail

while getopts "c:m:b:h" OPT; do
    case ${OPT} in
        c)
            COMMAND_LINE=${OPTARG}
            ;;
        m)
            MIME_TYPE=${OPTARG}
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

MIMEAPPS=${HOME}/.local/share/applications/mimeapps.list

BLOCK_ID=${BLOCK_ID:-ad1ac7e0-b4c3-4661-a73a-88f6b4fd6f92-xdg-open-for-${MIME_TYPE}}

mkdir -p $(dirname "${MIMEAPPS}")
insert-text-block "# ${BLOCK_ID}" "${MIMEAPPS}" <<EOF
[Added Associations]
${MIME_TYPE}=${APP_NAME}.desktop
EOF

DESKTOP_ENTRY=${HOME}/.local/share/applications/${APP_NAME}.desktop

insert-text-block "# ${BLOCK_ID}" ${DESKTOP_ENTRY} <<EOF
[Desktop Entry]
Exec=${COMMAND_LINE}
Terminal=false
Type=Application
MimeType=${MIME_TYPE}
EOF

sudo ln -sf ${DESKTOP_ENTRY} /usr/share/applications

xdg-mime default $(basename ${DESKTOP_ENTRY}) "${MIME_TYPE}"
xdg-mime query default "${MIME_TYPE}" | grep -F "${APP_NAME}"
