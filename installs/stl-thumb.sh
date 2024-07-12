#!/bin/bash -x

set -euo pipefail

# sudo apt-get install -y cargo
cd "$(realpath $(dirname "${BASH_SOURCE[0]}"))"
command -v cargo || ./cargo.sh

URL=https://github.com/unlimitedbacon/stl-thumb
REPO=${HOME}/git/$(basename "${URL}")
test -d "${REPO}" || git clone "${URL}" "${REPO}"

cd "${REPO}"
cargo install cargo-deb #this is an additional dependency
cargo deb

sudo apt install ./stl-thumb_.deb
