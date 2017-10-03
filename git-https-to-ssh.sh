#!/bin/bash -x
# echo 'https://github.com/erjoalgo/localForage.git' |
REMOTE="${1:-origin}"
SSH_URL=$(git remote -vv | grep "${REMOTE}" | head -1 | awk '{print $2}' | sed 's,https*://,ssh://git@,')
git remote rename "${REMOTE}" "${REMOTE}-https"
git remote add "${REMOTE}" "${SSH_URL}"
BRANCH=$(git rev-parse --abbrev-ref HEAD)
git branch --set-upstream-to=${REMOTE}/${BRANCH}
