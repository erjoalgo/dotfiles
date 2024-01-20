#!/bin/bash -x

set -euo pipefail

URL=https://github.com/LedgerHQ/app-passwords
REPO=${HOME}/git/$(basename "${URL}")
test -d "${REPO}" || git clone "${URL}" "${REPO}"
cd "${REPO}"

speculos-app-build.sh
