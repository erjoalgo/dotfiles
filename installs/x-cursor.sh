#!/bin/bash -x

set -euo pipefail

while getopts "d:h" OPT; do
    case ${OPT} in
    d)
        CURSOR_DIR=${OPTARG}
        ;;
    h)
        less "$0"
        exit 0
        ;;
    *)
        echo "unrecognized flag: ${OPT}" && exit ${LINENO}
        ;;
    esac
done
shift $((OPTIND -1))

if test -z "${CURSOR_DIR:-}"; then
    CURSOR_DIR=${1} && shift
fi

test -d "${CURSOR_DIR}"

CURSOR_THEME=$(grep -Po '(?<=Inherits=).*' "${CURSOR_DIR}/cursor.theme")

DEST="/usr/share/icons/${CURSOR_THEME}"

if ! test -d "${DEST}"; then
    sudo cp -r "${CURSOR_DIR}" "${DEST}"
fi

sudo sed -i "s/^\(Inherits=\).*/\1${CURSOR_THEME}/" \
     /usr/share/icons/default/index.theme

sed -i "s/^\(Xcursor.theme:\).*/\1${CURSOR_THEME}/" \
    "${HOME}/.Xresources"

sed -i \
    "s/^\(gtk-cursor-theme-name=\).*/\1\"${CURSOR_THEME}\"/" \
    ${HOME}/.gtkrc-*

insert-text-block '# 9a3b6e8c-6e2f-4b3f-b6e8-e4de4d3733bd-gtk-cursor-theme-name'  \
                  "${HOME}/.config/gtk-3.0/settings.ini"<<EOF
[Settings]
gtk-cursor-theme-name=${CURSOR_THEME}
EOF

for FILENAME in "${HOME}/.xinitrc" "${HOME}/.profile-env"; do
    insert-text-block  \
    '# 97995abf-83de-403d-b9d0-8da0347f3b5b-xinitrc-xcursor-name' \
    "${FILENAME}" -b <<EOF
export XCURSOR_THEME=${CURSOR_THEME}
export XCURSOR_PATH=${DEST}
EOF
done
