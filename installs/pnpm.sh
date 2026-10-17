#!/bin/bash -x

set -euo pipefail

cd "$(realpath $(dirname "${BASH_SOURCE[0]}"))"

source ~/.bashrc

if ! command -v npm; then
    ./nodenv.sh
    command -v npm
fi

# If you don't have Corepack yet (Node 25+ no longer bundles it by default)
npm install -g corepack

corepack enable
corepack prepare pnpm@latest --activate
