#!/bin/bash -x

set -euo pipefail
NVM_SH="${HOME}/.nvm/nvm.sh"

cd
test -d .nvm || git clone https://github.com/creationix/nvm.git .nvm

for PROFILE_FILE in \
    /etc/profile.d/node-env.sh \
        ${HOME}/.profile-env \
    ; do
    if test -w ${PROFILE_FILE}; then
        SUDOOPT=""
    else
        SUDOOPT="sudo"
    fi
    set +e
    ${SUDOOPT} insert-text-block \
               '# 69596022-9179-4a5c-be28-a6d12bcdc132-install-nvm' \
               ${PROFILE_FILE} <<EOF
export NVM_DIR="\$HOME/.nvm"
function nvmload  {
    echo "loading nvm"
    # This loads nvm
    [ -s "\$NVM_DIR/nvm.sh" ] && \. "\$NVM_DIR/nvm.sh" --no-use
    # This loads nvm bash_completion
    [ -s "\$NVM_DIR/bash_completion" ] && \. "\$NVM_DIR/bash_completion"
    nvm install stable
    # nvm alias default node
    NODE_DIR=\$(dirname \$(which node))
    insert-text-block \
      '# DAb44TrrbZ9qvuqy3C2He81kPX0NzUJB-export-current-node-location' \
       ${PROFILE_FILE} <<EOFF
export PATH=\\\$PATH:\${NODE_DIR}
EOFF
    echo "Added \${NODE_DIR} to PATH in ${PROFILE_FILE}"
}
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
source ${HOME}/.profile-env
nvmload

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
