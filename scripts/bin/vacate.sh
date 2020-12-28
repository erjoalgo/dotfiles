#!/bin/bash

set -euo pipefail

while getopts "dh" OPT; do
    case ${OPT} in
    d)
        export IGNORE_DANGLING_COMMITS=true
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))

# consider directories in reverse mtime order, so that newly-edited/fixed repos
# are considered last
for DIR in $(find ${HOME}/git/ /opt/git -maxdepth 1 -mindepth 1 -type d 2> /dev/null |  \
                 xargs ls -1trd) ${*}; do
    if test -n "${SKIP_NON_GIT_REPOS:-}" -a ! -d "${DIR}"; then
        continue
    fi
    cd "${DIR}"
    if ! git status &> /dev/null; then
        if  test -n "${SKIP_NON_GIT_REPOS:-}"; then
            continue;
        else
            echo "non git repository: ${DIR}"
            exit ${LINENO}
        fi
    fi
    if ! git-repo-is-safe-to-delete.sh; then
        echo "git repo at ${DIR} is not safe to delete"
        exit ${LINENO}
    fi
done
