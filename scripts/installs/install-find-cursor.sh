#!/bin/bash -x

set -euo pipefail

if ! command -v find-cursor; then
    URL=https://github.com/arp242/find-cursor
    GIT_REPO="${HOME}/git/$(basename ${URL})"
    if ! test -d "${GIT_REPO}"; then
        git clone "${URL}" "${GIT_REPO}"
    fi
    cd "${GIT_REPO}"
    make
    sudo make install
fi

command -v find-cursor
