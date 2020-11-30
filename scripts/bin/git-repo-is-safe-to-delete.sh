#!/bin/bash

set -eu #o pipefail

git status &> /dev/null


# check all remotes are up, since data pushed only to dead remotes will be lost
test "${GIT_NO_FETCH:-}" = true || git fetch --all

LOST=""

REMOVED=$(git ls-files --others --exclude-standard | tee /dev/stderr | wc -l)
if test ${REMOVED} != 0; then
   LOST+=", ${REMOVED} untracked files"
fi

UNSTAGED=$(git diff --shortstat)
if test -n "${UNSTAGED}"; then
   LOST+=", unstaged changes: ${UNSTAGED}"
fi

STAGED_LINES=$(git diff --cached | wc -l)
if test ${STAGED_LINES}  != 0; then
   LOST+=", ${STAGED_LINES} lines of staged changes"
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

DANGLING_COMMITS=$(git fsck --no-reflogs | grep -v 'dangling blob' | wc -l)
if test ${STASHED} != 0 -a -z "${IGNORE_DANGLING_COMMITS:-}"; then
    LOST+=", ${DANGLING_COMMITS} dangling commits"
fi

# explicitly-ignored files, warning only
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
