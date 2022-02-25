#!/bin/bash

set -euo pipefail

CONFIG_FILE_PATH=${1} && shift

PACKAGES=$(dpkg -S "${CONFIG_FILE_PATH}" | cut -f1 -d:)

if test $(wc -l <<< "${PACKAGES}") = 1; then
    PACKAGE="${PACKAGES}"
else
    select OPT in "${PACKAGES[@]}"; do
        break
    done
    PACKAGE=${OPT}
fi

TEMPDIR=$(mktemp -d)
cd "${TEMPDIR}"
apt-get download "${PACKAGE}"
dpkg-deb --fsys-tarfile *.deb > pkg.tar
tar axf pkg.tar

ORIGINAL=$(pwd)${CONFIG_FILE_PATH}

colordiff "${ORIGINAL}" "${CONFIG_FILE_PATH}" || true

read -p "Restore ${CONFIG_FILE_PATH} from original ${ORIGINAL}? "

sudo cp ${ORIGINAL} ${CONFIG_FILE_PATH}

rm -rf "${TEMPDIR}"
