#!/bin/bash

# send mail to the current user in case there exist no remote
# for which the current branch of this repo is up to date

# instead of sending mail, simply output those 'ahead' repos
# let cron send the mail automatically
# 30  7	* * *	root	~ealfonso/git/erjoalgo-gnu-scripts/git-notify.sh ~ealfonso/git/*


while test $# -gt 0; do
    REPO=${1}
    shift

    cd "${REPO}" || exit ${LINENO}

    # if ! git status &> /dev/null; then
    # if ! test -d .git; then
    if ! git status &> /dev/null; then
	# echo "not at a git repo: ${REPO}" && continue
	continue
    fi

    BRANCH=$(git rev-parse --abbrev-ref HEAD 2> /dev/null)

    if test $? -ne 0 || test $(git branch -r --contains ${BRANCH} | wc -l) -eq 0; then
	echo "${REPO}/${BRANCH} might be ahead of all remotes"
    fi
    # read
done

