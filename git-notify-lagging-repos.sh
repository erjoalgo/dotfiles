#!/bin/bash

# send mail to the current user in case there exist no remote
# for which the current branch of this repo is up to date

# instead of sending mail, simply output those 'ahead' repos
# let cron send the mail automatically
# 30  7	* * *	root	~ealfonso/git/erjoalgo-gnu-scripts/git-notify.sh ~ealfonso/git/*

# echo "git-notify called with args: $0 $*, PATH: $PATH, USER: $(whoami)"  \
#     | sudo tee /var/log/git-notify

function check_lagging	{
    test -d ${REPO} && cd "${REPO}" || return 2

    git status &> /dev/null || return 2
    BRANCH=$(git rev-parse --abbrev-ref HEAD 2> /dev/null) || return 2

    TAGS=""
    if test $? -ne 0 -o $(git branch -r --contains ${BRANCH} | wc -l) -eq 0; then
	TAGS+=" UNPUBLISHED"
	echo "$(basename ${REPO})/${BRANCH} "
    fi
    if ! git diff --exit-code >/dev/null|| ! git diff --cached --exit-code >/dev/null; then
	TAGS+=" NOT-COMMITED"
    fi
    # check stash
    if test $(git ls-files --others --exclude-standard | wc -l) -ne 0; then
	TAGS+=" UNTRACKED"
    fi

    if test -n "${TAGS}"; then
	echo "$(basename ${REPO})/${BRANCH} ${TAGS}"
    fi
}

RED='\033[0;31m'
NC='\033[0m' # No Color
# https://stackoverflow.com/questions/5947742/

while test $# -gt 0; do
    REPO=${1} && shift
    check_lagging "${REPO}"
    LAST=$?
    if test ${LAST} -ne 1; then
	printf "${RED}problem processing ${REPO} ...${NC}\n" 1>&2
    fi
done
