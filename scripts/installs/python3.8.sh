#!/bin/bash -x

set -euo pipefail

sudo apt-get install -y libffi-dev liblzma-dev \
     build-essential zlib1g-dev libncurses5-dev \
     libgdbm-dev libnss3-dev libssl-dev libsqlite3-dev libreadline-dev libffi-dev curl  \
     libbz2-dev


if ! command -v python3.9; then
    install-from-source  \
        -u https://www.python.org/ftp/python/3.9.0/Python-3.9.0.tar.xz \
        -c "--enable-optimizations"
fi
