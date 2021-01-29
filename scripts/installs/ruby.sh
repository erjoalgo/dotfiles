#!/bin/bash -x

set -euo pipefail

PROFILE_ENV=${HOME}/.profile-env

# Install rbenv locally for the dev user
if ! test -d ~/.rbenv; then
  git clone https://github.com/rbenv/rbenv.git ~/.rbenv
  # Optional: Compile bash extensions
  cd ~/.rbenv && src/configure && make -C src
  # Add rbenv to the shell's $PATH.
fi
insert-text-block '# 088ef3e6-c08d-47b0-9697-e21d74d63b57-add-rbenv-path'  \
  ${PROFILE_ENV} <<EOF
  export PATH="\${PATH}:${HOME}/.rbenv/bin"
EOF

source "${PROFILE_ENV}"
command -v rbenv

# Run rbenv-init and follow the instructions to initialize rbenv on any shell
# echo "# Source bashrc: source ~/.bashrc"

if ! test -d ~/.rbenv/plugins/ruby-build; then
  git clone https://github.com/rbenv/ruby-build.git ~/.rbenv/plugins/ruby-build
fi

source ${PROFILE_ENV}

rbenv init || true

eval "$(rbenv init -)"

# rvm get stable
# rvm autolibs enable
# rvm reinstall all --force

INSTALL_VERSION=${INSTALL_VERSION:-$(rbenv install -l | grep -v - | tail -1)}

insert-text-block '# 3d2182a9-52d4-41cb-bc91-59a51293e89a--add-nodenv-to-path'  \
  "${HOME}/.bashrc"<<EOF
eval "\$(rbenv init -)"
EOF

sudo apt-get install -y libssl-dev
rbenv install -s ${INSTALL_VERSION}

rbenv global ${INSTALL_VERSION}

rbenv rehash
sudo gem install bundler --conservative
