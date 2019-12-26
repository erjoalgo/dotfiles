#!/bin/bash

set -eu #o pipefail

git status &> /dev/null


# https://stackoverflow.com/questions/35311231/

LOST=""

# what local files would be lost forever if I deleted this repo?
REMOVED=$(git ls-files --others --exclude-standard | tee /dev/stderr | wc -l)
if test ${REMOVED} != 0; then
   LOST+=", ${REMOVED} untracked files"
fi

LINES=$(git diff | wc -l)
if test ${LINES}  != 0; then
   LOST+=", ${LINES} lines of unstaged changes"
fi

LINES=$(git diff --cached | wc -l)
if test ${LINES}  != 0; then
   LOST+=", ${LINES} lines of staged changes"
fi

# git fetch origin
# https://stackoverflow.com/questions/2016901/viewing-unpushed-git-commits
LOCAL_COMMITS=$(git --no-pager log --branches --not --remotes --oneline | wc -l)
if test ${LOCAL_COMMITS} != 0; then
    LOST+=", ${LOCAL_COMMITS} local unpublished commits (via git log)"
fi

STASHED=$(git --no-pager stash list | tee /dev/stderr | wc -l)
if test ${STASHED} != 0; then
    LOST+=", ${STASHED} stashed changes"
fi


IGNORED=$(git clean -ndX | wc -l)
if test ${IGNORED} != 0; then
    echo "WARN: ${IGNORED} ignored files would be lost in $(pwd)"
fi

if test -n "${LOST}"; then
    MESSAGE=$(sed 's/, \(, \)*/\n - /g' <<< "${LOST}")
    echo "WARN: data would be lost if $(pwd) was deleted: "
    echo "${MESSAGE}"
    exit ${LINENO}
fi



# git push origin --all --dry-run 2>&1 | tee /dev/stderr \
#     | grep -F "Everything up-to-date"
