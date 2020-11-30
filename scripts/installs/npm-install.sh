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
    set +e
    ${SUDOOPT} insert-text-block \
               '# 69596022-9179-4a5c-be28-a6d12bcdc132-install-nvm' \
               ${PROFILE_FILE} <<"EOF"
export NVM_DIR="$HOME/.nvm"
function my_nmv_load  {
    echo "loading nvm"
    # This loads nvm
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" --no-use
    # This loads nvm bash_completion
    [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"
    unalias npm 2>/dev/null
    nvm install stable
    nvm alias default node
    npm ${*}
}
alias npm='my_nmv_load'

EOF
    STATUS=$?
    set -e
    if test ${STATUS} = 0; then
        NVM_PROFILE_INTSALLED=true
    fi
done

if ! test "${NVM_PROFILE_INTSALLED:-}" = true; then
    echo "failed to install nvm to a bash .profile" && exit ${LINENO}
fi

source "${NVM_SH}"

set +xu
nvm install stable
nvm alias default node
set -xu

if ! command -v npm; then
    npm --version
fi

cat<<EOF
NVM has been installed and added to profile. Now run:

source ${NVM_SH}
EOF
