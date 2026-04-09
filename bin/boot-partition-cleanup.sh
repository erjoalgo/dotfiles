#!/bin/bash -x

set -euo pipefail

BACKUP=/var/backups/boot/

sudo mkdir -p "${BACKUP}"

sudo rsync -rv /boot/ "${BACKUP}"

if ! sudo apt-get autoremove; then
    TO_REMOVE=$(dpkg --list 'linux-image*' | grep -Po '(?<=^ii) *[^ ]+' |  \
                    grep -v $(uname -r) | tr -d ' ')
    sudo apt-get remove ${TO_REMOVE}
fi


