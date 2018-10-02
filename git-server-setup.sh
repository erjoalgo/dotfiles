#!/bin/bash -x

set -euo pipefail

which git-shell

grep git-shell /etc/shells || which git-shell >> /etc/shells

cat /etc/passwd | grep ^git: ||  \
        sudo adduser git --disabled-password --shell $(which git-shell) \
        --gecos ",,,"

# double-check that git-shell is the shell
sudo chsh git -s $(which git-shell)

SRV_PREFIX=/opt/git
sudo bash -s <<EOF
set -xeuo pipefail

cd ~git
mkdir -p .ssh && cd .ssh
touch authorized_keys && chmod 644 authorized_keys
chown -R git:git .

mkdir -p ${SRV_PREFIX}
chown -R git:git ${SRV_PREFIX}
EOF

echo "success"
