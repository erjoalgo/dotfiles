#!/bin/bash -x

set -euo pipefail

URL=https://github.com/LedgerHQ/app-passwords
REPO=${HOME}/git/$(basename "${URL}")
test -d "${REPO}" || git clone "${URL}" "${REPO}"
cd "${REPO}"
git pull --ff-only

pwd

ELF=$(pwd)/build/nanos/bin/app.elf

test -e "${ELF}"

docker pull ghcr.io/ledgerhq/ledger-app-builder/ledger-app-dev-tools:latest

docker run --rm -it  \
       -v $(dirname "${ELF}"):/speculos/apps \
       --publish 5000:5000  \
       ghcr.io/ledgerhq/speculos \
       --display headless  \
       apps/app.elf
