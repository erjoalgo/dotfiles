#!/bin/bash -x

set -euo pipefail
cd
test -d .nvm || git clone https://github.com/creationix/nvm.git .nvm

insert-text-block '# 69596022-9179-4a5c-be28-a6d12bcdc132-install-nvm' ${HOME}/.profile <<"EOF"
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
EOF
