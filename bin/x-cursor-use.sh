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
test -d "${CURSOR_DIR}/cursors"

CURSOR_THEME=$(basename "${CURSOR_DIR}")

DEST="/usr/share/icons/${CURSOR_THEME}"

if ! test -d "${DEST}"; then
    sudo cp -r "${CURSOR_DIR}" "${DEST}"
fi

sudo sed -i "s/^\(Inherits=\).*/\1${CURSOR_THEME}/" \
     /usr/share/icons/default/index.theme

sed -i "s/^\(Xcursor.theme:\).*/\1 ${CURSOR_THEME}/" \
    "${HOME}/.Xresources"

sed -i \
    "s/^\(gtk-cursor-theme-name\)=.*/\1 = ${CURSOR_THEME}/" \
    ${HOME}/.gtkrc-*

for FILE in "${HOME}/.config/gtk-3.0/settings.ini" \
                "${HOME}/.config/gtk-2.0/settings.ini"; do
    insert-text-block  \
        '# 9a3b6e8c-6e2f-4b3f-b6e8-e4de4d3733bd-gtk-cursor-theme-name'  \
        "${FILE}" <<EOF
[Settings]
gtk-cursor-theme-name = ${CURSOR_THEME}
EOF
done

for FILENAME in "${HOME}/.xinitrc" "${HOME}/.profile-env"; do
    insert-text-block  \
    '# 97995abf-83de-403d-b9d0-8da0347f3b5b-xinitrc-xcursor-name' \
    "${FILENAME}" -b <<EOF
export XCURSOR_THEME=${CURSOR_THEME}
export XCURSOR_PATH=/usr/share/icons/
EOF
done

if command -v gsettings; then
    gsettings set org.gnome.desktop.interface cursor-theme ${CURSOR_THEME}
    gsettings set org.mate.peripherals-mouse cursor-theme ${CURSOR_THEME} || true
fi

for FILE in "${HOME}/.xprofile" "${HOME}/.xinitrc"; do
    insert-text-block '# 0e37c6bc-90c4-4613-9956-8112f4f5da45-xrdb-xresources'  \
                      "${FILE}" <<EOF
xrdb -merge ${HOME}/.Xresources
EOF
done

for DEST in  \
    "${HOME}/.local/share/icons/default" \
    "${HOME}/.icons/default"; do
    mkdir -p $(dirname "${DEST}")
    if test -L "${DEST}"; then
        unlink "${DEST}"
    fi
    ln -sf $(realpath "${CURSOR_DIR}") "${DEST}"
done
