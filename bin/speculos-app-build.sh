#!/bin/bash -x

set -euo pipefail

ELF=bin/app.elf
rm -f "${ELF}"
docker run --rm -ti --user "$(id -u):$(id -g)" -v "$(realpath .):/app" \
           ghcr.io/ledgerhq/ledger-app-builder/ledger-app-dev-tools:latest \
           make DEBUG=1
test -e "${ELF}"
echo "pulled $(pwd)/$(basename ${ELF})"
