#!/bin/bash -x

set -euo pipefail

# ensure we're on a git directory
git status

function usage {
    echo "$(basename ${0}) -t SSH_USERHOST [ -o SSH_opts ] -r REMOTE_NAME [ -n repo_name ]
[ -d remote_git_bare_prefix ]"
}

SSH_OPTS=""
while getopts "ht:o:r:n:d:" OPT; do
    case ${OPT} in
    t)
        SSH_USERHOST=${OPTARG}
        ;;
    o)
        SSH_OPTS=${OPTARG}
        ;;
    n)
        REPO=${OPTARG}
        ;;
    d)
        SRV_PREFIX=${OPTARG}
        ;;
    r)
        REMOTE_NAME=${OPTARG}
        ;;
    h)
        less $0
        usage
        exit 0
        ;;
    esac
done


REPO_NAME=${REPO_NAME:-$(basename $(pwd))}
SRV_PREFIX=${SRV_PREFIX:-/opt/git}
if test -z "${SSH_USERHOST}" -o -z "${REMOTE_NAME}"; then
    usage
    exit 1
fi

ssh ${SSH_USERHOST} ${SSH_OPTS} "sudo bash -s" <<EOF
set -e

REPO_PATH=${SRV_PREFIX}/${REPO_NAME}
mkdir -p \$REPO_PATH && cd \$REPO_PATH
git init --bare
chown -R git:git .
EOF

# TODO remove possible user from SSH_USERHOST
# TODO add possible non-standard port

REMOTE_URL="ssh://git@${SSH_USERHOST}${SRV_PREFIX}/${REPO_NAME}"
ORIGIN_NAME=origin

BRANCH=$(git rev-parse --abbrev-ref HEAD 2> /dev/null)
git fetch ${REMOTE_NAME}
git branch --set-upstream-to=${REMOTE_NAME}/${BRANCH}
