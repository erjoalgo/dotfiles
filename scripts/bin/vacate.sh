#!/bin/bash

set -euo pipefail

# consider directories in reverse mtime order, so that newly-edited/fixed repos
# are considered last
for DIR in $(find ${HOME}/git/ /opt/git -maxdepth 1 -mindepth 1 -type d |  \
                 xargs ls -1trd) ${*}; do
    if test -n "${ALLOW_NON_GIT_REPOS:-}" -a ! -d "${DIR}"; then
        continue
    fi
    echo "considering ${DIR}"
    cd "${DIR}"
    if ! git status &> /dev/null; then
        if  test -n "${ALLOW_NON_GIT_REPOS:-}"; then
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
