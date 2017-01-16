#!/bin/bash -x
# echo 'https://github.com/erjoalgo/localForage.git' |
BRANCH="${1:-origin}"
SSH_URL=$(git remote -vv | grep "${BRANCH}" | head -1 | awk '{print $2}' | sed 's,https://,ssh://git@,')
git remote set-url "${BRANCH}" "${SSH_URL}"
