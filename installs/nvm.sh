#!/bin/bash -x

set -euo pipefail

REPO=${HOME}/git/nvm
if ! test -d "${REPO}"; then
     git clone https://github.com/nvm-sh/nvm ${REPO}
fi

NVM_SH="${HOME}/.nvm/nvm.sh"
if ! test -e "${NVM_SH}"; then
    "${REPO}/install.sh"
fi

. "${NVM_SH}"

set +xu
nvm install stable
set -xu

which node
NODE_DIR=$(dirname $(which node))

insert-text-block \
      '# DAb44TrrbZ9qvuqy3C2He81kPX0NzUJB-export-current-node-location' \
       ${HOME}/.profile-env <<EOF
export PATH=${NODE_DIR}:\$PATH
EOF

npm config set prefix '~/.local/'
