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
