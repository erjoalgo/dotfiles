#!/bin/bash -x

set -euo pipefail

USERHOST=${1} && shift
PORT=${1:-22} && shift || true

REPO=$(basename $(pwd))
HOST=$(sed 's/.*@//g' <<< ${USERHOST})
SRV_PREFIX=/opt/git

CMD=$(cat <<EOF
EOF
)

ssh ${USERHOST} -p${PORT}  "sudo bash -s" <<EOF
set -e

REPOPATH=${SRV_PREFIX}/${REPO}
mkdir -p \$REPOPATH && cd \$REPOPATH
git init --bare
chown -R git:git .
EOF

REMOTE_URL="ssh://git@${HOST}:${PORT}${SRV_PREFIX}/${REPO}"
echo ${REMOTE_URL}
git remote add origin ${REMOTE_URL} || true
BRANCH=$(git rev-parse --abbrev-ref HEAD 2> /dev/null)
git fetch origin
git branch --set-upstream-to=origin/${BRANCH}
