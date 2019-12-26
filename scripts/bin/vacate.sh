#!/bin/bash

set -euo pipefail

for DIR in $(find ${HOME}/git/ /opt/git -maxdepth 1 -type d); do
    cd "${DIR}"
    pwd
    if git status &> /dev/null; then
        if ! git-repo-is-safe-to-delete.sh; then
            echo "git repo at ${DIR} is not safe to delete"
            exit ${LINENO}
        fi
    fi
done
