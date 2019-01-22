#!/bin/bash -x

set -euo pipefail

PROJECT_DIR=${1} && shift
SANDBOX_BIN=$(find "${PROJECT_DIR}" -path  \
                   "*/node_modules/puppeteer/.local-chromium/linux-*/chrome-linux/chrome_sandbox")
sudo chown root:root "${SANDBOX_BIN}"
sudo chmod 4755 "${SANDBOX_BIN}"
# copy sandbox executable to a shared location
BINDIR=/usr/local/sbin/
sudo cp -p "${SANDBOX_BIN}" "${BINDIR}/chrome-devel-sandbox"

insert-text-block '# 23c70cf5-1d79-4890-a78a-0715db0251bf-add-chrome-devel-sandbox-envvar'  \
                  ${HOME}/.profile-env <<EOF
export CHROME_DEVEL_SANDBOX=${BINDIR}/chrome-devel-sandbox
EOF

# Local Variables:
# compile-command: "./chrome-sandbox.sh ~/git/product-cmp/puppeteer/"
# End:
