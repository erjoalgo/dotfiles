#!/bin/bash -x

set -euo pipefail

sudo apt-get install -y libffi-dev

if ! command -v python3.8 || test -n "${FORCE:-}"; then
    install-from-source \
        -u https://www.python.org/ftp/python/3.8.3/Python-3.8.3.tar.xz
fi

