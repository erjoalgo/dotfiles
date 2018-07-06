#!/bin/bash

# send mail to the current user in case there exist no remote
# for which the current branch of this repo is up to date

# instead of sending mail, simply output those 'ahead' repos
# let cron send the mail automatically
# 30  7	* * *	root	~ealfonso/git/erjoalgo-gnu-scripts/git-notify.sh ~ealfonso/git/*

# echo "git-notify called with args: $0 $*, PATH: $PATH, USER: $(whoami)"  \
#     | sudo tee /var/log/git-notify

# TODO
# #  flag to check if remote has more recent changes
# #  iterate over all branches, notify unpublished branches
# #  # is this possible?

BLACK='\033[0;30m'
DARK_GRAY='\033[1;30m'
RED='\033[0;31m'
LIGHT_RED='\033[1;31m'
GREEN='\033[0;32m'
LIGHT_GREEN='\033[1;32m'
BROWN_ORANGE='\033[0;33m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
LIGHT_BLUE='\033[1;34m'
PURPLE='\033[0;35m'
LIGHT_PURPLE='\033[1;35m'
CYAN='\033[0;36m'
LIGHT_CYAN='\033[1;36m'
LIGHT_GRAY='\033[0;37m'
WHITE='\033[1;37m'

function check_lagging	{
    test -d "${REPO}" && cd "${REPO}" || return 2

    if ! git status &> /dev/null; then
	printf "${DARK_GRAY}$(basename ${REPO}) not a git repo?${NC}\n"
	return
    fi

    BRANCH=$(git rev-parse --abbrev-ref HEAD 2> /dev/null)
    if test 0 -ne $?; then
	printf "${DARK_GRAY}$(basename ${REPO}) has no commits?${NC}\n"
	return
    fi

    TAGS=""
    REMOTE=$(git config branch.${BRANCH}.remote)
    if test -z "${REMOTE}"; then
	TAGS+=" ${YELLOW}NO-REMOTE${NC}"
    else
	if test 0 -eq $(git branch -r --contains ${BRANCH} | wc -l); then
	    CNT=$(git log ${REMOTE}/${BRANCH}..HEAD --oneline | wc -l)
	    TAGS+=" ${LIGHT_RED}${CNT}-UNPUBLISHED${NC}"
	fi
	if test "${FETCH}" = true; then
	    git fetch ${REMOTE}
	fi
	CNT=$(git log HEAD..${REMOTE}/${BRANCH} --oneline | wc -l)
	if test 0 -ne ${CNT}; then
	    TAGS+=" ${BLUE}${CNT}-BEHIND-REMOTE${NC}"
	fi
    fi
    if ! git diff --exit-code >/dev/null|| ! git diff --cached --exit-code >/dev/null; then
	TAGS+=" ${BROWN_ORANGE}UNCOMMITED${NC}"
    fi

    STASH_CNT=$(git stash list | wc -l)
    if test "${STASH_CNT}" -gt 0; then
	TAGS+=" ${YELLOW}${STASH_CNT}-STASHED${NC}"
    fi
    UNTRACKED_CNT=$(git ls-files --others --exclude-standard | wc -l)
    if test ${UNTRACKED_CNT} -ne 0; then
	TAGS+=" ${DARK_GRAY}${UNTRACKED_CNT}-UNTRACKED${NC}"
    fi

    if test -n "${TAGS}"; then
	printf "$(basename ${REPO})/${BRANCH} ${TAGS}\n"
    fi
}

RED='\033[0;31m'
NC='\033[0m' # No Color
# https://stackoverflow.com/questions/5947742/

ORIGPWD=$(pwd)
while test $# -gt 0; do
    cd "${ORIGPWD}"
    REPO=${1} && shift
    check_lagging "${REPO}"
done
