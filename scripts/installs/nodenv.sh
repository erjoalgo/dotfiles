#!/bin/bash -x

set -euo pipefail

# Install nodenv
test -d ~/.nodenv || git clone https://github.com/nodenv/nodenv.git ~/.nodenv

# Optional: Install bash extensions
cd ~/.nodenv && src/configure && make -C src

# Add nodenv to the shell's $PATH.
PROFILE_ENV=${HOME}/.profile-env
insert-text-block '# 135deb13-3651-424f-9214-a0989b1c9ee3-add-nodenv-to-path'  \
  "${PROFILE_ENV}"<<EOF
export PATH="\${PATH}:${HOME}/.nodenv/bin"
EOF

insert-text-block '# 7688cf02-3806-4328-9695-2231e6b32d7d--init-nodenv'  \
  "${HOME}/.bashrc"<<EOF
eval "\$(nodenv init -)"
EOF

source ${PROFILE_ENV}

# Run nodenv init and follow the instructions to initialize nodenv on any shell
eval "$(nodenv init -)"

NODE_BUILD_ROOT=$(nodenv root)/plugins/node-build
if ! test -d "${NODE_BUILD_ROOT}"; then
  git clone https://github.com/nodenv/node-build.git ${NODE_BUILD_ROOT}
fi

INSTALL_VERSION=${INSTALL_VERSION:-$(nodenv install -l | grep '^[0-9.]*$' | tail -1)}

nodenv install -s "${INSTALL_VERSION}"
nodenv global "${INSTALL_VERSION}"
nodenv rehash

command -v node

command -v npm
