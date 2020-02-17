#!/bin/bash -x

set -euo pipefail
NVM_SH="${HOME}/.nvm/nvm.sh"

cd
test -d .nvm || git clone https://github.com/creationix/nvm.git .nvm

for PROFILE_FILE in \
    /etc/profile.d/node-env.sh \
        ${HOME}/.profile-env \
    ; do
    test -w ${PROFILE_FILE} && SUDOOPT="" || SUDOOPT="sudo"
    ${SUDOOPT} insert-text-block \
               '# 69596022-9179-4a5c-be28-a6d12bcdc132-install-nvm' \
               ${PROFILE_FILE} <<"EOF"
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" --no-use # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
EOF
done
source "${NVM_SH}"

nvm install stable
nvm alias default node

if ! command -v npm; then
    npm --version
fi

cat<<EOF
NVM has been installed and added to profile. Now run:

source ${NVM_SH}
EOF
