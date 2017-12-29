#!/bin/bash -x

set -euo pipefail

git status
USERHOST=${1} && shift
PORT=${1:-22} && shift || true

REPO=$(basename $(pwd))
HOST=$(sed 's/.*@//g' <<< ${USERHOST})
SRV_PREFIX=/opt/git

# git server setup. ensure git user has been set up with right shell
RSA=$(cat ${HOME}/.ssh/id_rsa.pub)

ssh ${USERHOST} -p${PORT}  "sudo bash -s" <<EOF
which git-shell

grep git-shell /etc/shells || which git-shell >> /etc/shells

cat /etc/passwd | grep ^git: ||  \
	sudo adduser git --disabled-password --shell $(which git-shell) \
	--gecos ",,,"

# double-check that git-shell is the shell
sudo chsh git -s $(which git-shell)

KEYS=~git/.ssh/authorized_keys
# sudo -u git ssh-keygen
sudo -u git mkdir ~git/.ssh
echo "${RSA}" | sudo -u git tee -a \${KEYS}
chmod 644 \${KEYS}
EOF

ssh ${USERHOST} -p${PORT}  "sudo bash -s" <<EOF
set -e

REPOPATH=${SRV_PREFIX}/${REPO}
mkdir -p \$REPOPATH && cd \$REPOPATH
git init --bare
chown -R git:git .
EOF

REMOTE_URL="ssh://git@${HOST}:${PORT}${SRV_PREFIX}/${REPO}"
echo ${REMOTE_URL}
ORIGIN_NAME=origin
for NAME in origin origin-${USERHOST} origin-${USERHOST}-${RANDOM}; do
    ORIGIN_NAME=${NAME}
    if test $(git config --get remote.${NAME}.url) = ${REMOTE_URL}; then
	break
    elif git remote add ${ORIGIN_NAME} ${REMOTE_URL}; then
	break
    fi
done


BRANCH=$(git rev-parse --abbrev-ref HEAD 2> /dev/null)
git fetch ${ORIGIN_NAME}
git branch --set-upstream-to=${ORIGIN_NAME}/${BRANCH}
