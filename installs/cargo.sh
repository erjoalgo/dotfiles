#!/bin/bash -x

set -euo pipefail

SCRIPT=/tmp/cargo-installer.sh
curl https://sh.rustup.rs -sSf > "${SCRIPT}"

less "${SCRIPT}"

chmod +x "${SCRIPT}"
"${SCRIPT}"
