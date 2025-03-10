#!/bin/bash -x

set -euo pipefail

# ensure we're on a git directory
git status

function usage {
    echo "$(basename ${0}) -t SSH_USERHOST [-r REMOTE_NAME] [ -o SSH_opts ] [ -n repo_name ]
[ -d remote_git_bare_prefix ]"
}

SSH_OPTS=""
PORT_OPT=""
SKIP_GIT_SETUP=""
SET_UPSTREAM_OPT="--set-upstream"
while getopts "ht:p:r:n:d:kaU" OPT; do
    case ${OPT} in
    t)
        SSH_USERHOST=${OPTARG}
        ;;
    p)
        SSH_OPTS+=" -p${OPTARG} "
        PORT_OPT+=:${OPTARG}
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
    k)
        SKIP_GIT_SETUP=true
        ;;
    a)
        AFS=true
        SET_UPSTREAM_OPT=""
        REMOTE_NAME=afs
        ;;
    U)
        SET_UPSTREAM_OPT=""
        ;;
    h)
        less $0
        usage
        exit 0
        ;;
    esac
done


REPO_NAME=${REPO_NAME:-$(basename $(git rev-parse --show-toplevel))}
SRV_PREFIX=${SRV_PREFIX:-/opt/git}
if test -z "${SSH_USERHOST:-}" -a -z "${AFS:-}"; then
    usage
    exit 1
fi

if test -z "${REMOTE_NAME:-}"; then
    if ! git config --get "remote.origin"; then
        REMOTE_NAME=origin
    else
        REMOTE_NAME="${SSH_USERHOST}"
    fi
fi

if test -z "${AFS:-}"; then
    REMOTE_URL="ssh://git@${SSH_USERHOST}${PORT_OPT}${SRV_PREFIX}/${REPO_NAME}"
else
    REMOTE_URL="/afs/$(tr -d '\n' < /etc/openafs/ThisCell)/home/${USER}/git-bare/${REPO_NAME}"
fi

if test -z "${SKIP_GIT_SETUP}"; then
    if test -z "${AFS:-}"; then
        ssh -vv ${SSH_USERHOST} ${SSH_OPTS} sudo bash -xs <<EOF
set -euxo pipefail

REPO_PATH=${SRV_PREFIX}/${REPO_NAME}
mkdir -p \$REPO_PATH
git init --bare \$REPO_PATH
chown -R git:git \$REPO_PATH
chmod -R g=u \$REPO_PATH
EOF
    else
        mkdir -p "${REMOTE_URL}"
        git init --bare "${REMOTE_URL}"
    fi
fi

# TODO remove possible user from SSH_USERHOST
# TODO add possible non-standard port

function git-remote-url {
    REMOTE=${1} && shift
    git config --get "remote.${REMOTE}.url"
}

if ! git-remote-url "${REMOTE_NAME}"; then
   git remote add "${REMOTE_NAME}" "${REMOTE_URL}"
elif test $(git-remote-url "${REMOTE_NAME}") != "${REMOTE_URL}"; then
    echo "remote ${REMOTE_NAME} exists and doesn't point to ${REMOTE_URL}"
    OLD=${REMOTE_NAME}
    while git config --get "remote.${OLD}.url"; do
        OLD+=.old
    done
    echo "renaming ${REMOTE_NAME} to ${OLD}"
    git remote rename ${REMOTE_NAME} ${OLD}
    git remote add "${REMOTE_NAME}" "${REMOTE_URL}"
fi

echo "added remote ${REMOTE_NAME} as ${REMOTE_URL}"

git fetch ${REMOTE_NAME}

if ! git log > /dev/null; then
    echo "error: missing initial commit" && exit ${LINENO}
fi

BRANCH=$(git rev-parse --abbrev-ref HEAD 2> /dev/null) || true
git push ${SET_UPSTREAM_OPT} ${REMOTE_NAME} ${BRANCH:-}

