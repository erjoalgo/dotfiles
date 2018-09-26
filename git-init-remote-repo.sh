#!/bin/bash -x

set -euo pipefail

# ensure we're on a git directory
git status

function usage {
    echo "$(basename ${0}) -t SSH_USERHOST [ -o SSH_opts ] [ -r local_repo ]
[ -d remote_git_bare_prefix ]"
}

SSH_OPTS=""
while getopts "ht:o:r:d:" OPT; do
    case ${OPT} in
    t)
        SSH_USERHOST=${OPTARG}
        ;;
    o)
        SSH_OPTS=${OPTARG}
        ;;
    r)
        REPO=${OPTARG}
        ;;
    d)
        SRV_PREFIX=${OPTARG}
        ;;
    h)
        less $0
        usage
        exit 0
        ;;
    esac
done


REPO=${REPO:-$(basename $(pwd))}
SRV_PREFIX=${SRV_PREFIX:-/opt/git}
if test -z "${SSH_USERHOST}"; then
    usage
    exit 1
fi

ssh ${SSH_USERHOST} ${SSH_OPTS}  "sudo bash -s" <<EOF
set -e

REPOPATH=${SRV_PREFIX}/${REPO}
mkdir -p \$REPOPATH && cd \$REPOPATH
git init --bare
chown -R git:git .
EOF

# TODO remove possible user from SSH_USERHOST
# TODO add possible non-standard port

REMOTE_URL="ssh://git@${SSH_USERHOST}${SRV_PREFIX}/${REPO}"
echo ${REMOTE_URL}
ORIGIN_NAME=origin
for NAME in origin origin-${SSH_USERHOST} origin-${SSH_USERHOST}-${RANDOM}; do
    ORIGIN_NAME=${NAME}
    if test "$(git config --get remote.${NAME}.url)" = "${REMOTE_URL}"; then
	break
    elif git remote add ${ORIGIN_NAME} ${REMOTE_URL}; then
	break
    fi
done

BRANCH=$(git rev-parse --abbrev-ref HEAD 2> /dev/null)
git fetch ${ORIGIN_NAME}
git branch --set-upstream-to=${ORIGIN_NAME}/${BRANCH}
