#!/bin/bash -x

set -euo pipefail

EMAILS=${1:-erjoalgo@gmail.com}

BARE=/tmp/git-bare-notif-test.git
mkdir -p ${BARE}
cd ${BARE}
git init --bare

# REPO=$(dirname ${BARE})/$(basename "${BARE}" .git)
REPO=$(dirname ${BARE})/$(basename "${BARE}" .git)
test -d ${REPO} || git clone ${BARE} ${REPO}
cd ${REPO}

git-bare-repo-add-post-receive-email-notification.sh ${BARE} ${EMAILS}

pwd
echo ${RANDOM} >> file
git add file
git commit -m 'test commit'
git push origin master
