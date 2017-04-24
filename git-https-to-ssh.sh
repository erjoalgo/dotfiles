#!/bin/bash -x
# echo 'https://github.com/erjoalgo/localForage.git' |
REMOTE="${1:-origin}"
SSH_URL=$(git remote -vv | grep "${REMOTE}" | head -1 | awk '{print $2}' | sed 's,https://,ssh://git@,')
git remote set-url "${REMOTE}" "${SSH_URL}"
