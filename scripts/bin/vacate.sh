#!/bin/bash

set -euo pipefail

# consider directories in reverse mtime order, so that newly-edited/fixed repos
# are considered last
for DIR in $(find ${HOME}/git/ /opt/git -maxdepth 1 -type d | xargs ls -1trd); do
    cd "${DIR}"
    pwd
    if git status &> /dev/null; then
        if ! git-repo-is-safe-to-delete.sh; then
            echo "git repo at ${DIR} is not safe to delete"
            exit ${LINENO}
        fi
    fi
done
