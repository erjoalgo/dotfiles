#!/bin/bash -x

set -euo pipefail

cd "$( dirname "${BASH_SOURCE[0]}" )"

# set up udev ledger permissions, etc

./live.sh

pip3 install ledger_agent

insert-text-block '# 6c3fa738-ee9b-41b2-be6d-c44221944bbf-set-gnupg-home' \
                  ${HOME}/.profile-env <<EOF
GNUPGHOME=${HOME}/.gnupg/ledger
EOF

if ! test -d ${HOME}/.gnupg/ledger; then
    ledger-gpg init "Ernesto Alfonso <erjoalgo@gmail.com>" --time=0
fi
