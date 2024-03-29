#!/bin/bash -x

REMOTE="${1:-origin}"
SSH_URL=$(git remote -vv |  \
              grep "${REMOTE}" |  \
              head -1 |  \
              awk '{print $2}' |  \
              sed 's,https*://,ssh://git@,')
git remote set-url "${REMOTE}" "${SSH_URL}"
BRANCH=$(git rev-parse --abbrev-ref HEAD)
git fetch ${REMOTE}
git branch --set-upstream-to=${REMOTE}/${BRANCH}
