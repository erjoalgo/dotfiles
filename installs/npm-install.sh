#!/bin/bash -x

set -euo pipefail
NVM_SH="${HOME}/.nvm/nvm.sh"

cd
test -d .nvm || git clone https://github.com/creationix/nvm.git .nvm


NVM_LOAD=${HOME}/.nvm-load
cat <<EOF > ${NVM_LOAD}
export NVM_DIR="\$HOME/.nvm"
nvmload()  {
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
       ${HOME}/.profile-env <<EOFF
export PATH=\\\$PATH:\${NODE_DIR}
EOFF
    npm config set prefix '~/.local/'
}
EOF

NVM_PROFILE_INTSALLED=true

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
. ${NVM_LOAD}
EOF
    STATUS=$?
    set -e
    if test ${STATUS} != 0; then
        NVM_PROFILE_INTSALLED=false
    fi
done

if ! test "${NVM_PROFILE_INTSALLED:-}" = true; then
    echo "failed to install nvm to a bash .profile" && exit ${LINENO}
fi

. "${NVM_SH}"
. ${HOME}/.profile-env
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
