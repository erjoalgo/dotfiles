#!/bin/bash

set -euo pipefail

for DIR in ${HOME}/git/*; do
    cd "${DIR}"
    pwd
    if git status &> /dev/null; then
        if ! git-repo-is-safe-to-delete.sh; then
            echo "git repo at ${DIR} is not safe to delete"
            exit ${LINENO}
        fi
    fi
done
