#!/bin/bash -x

set -euo pipefail

WRONG_EMAIL=$1 && shift
NEW_EMAIL=$1 && shift

git filter-branch -f --env-filter "
if [ \$GIT_COMMITTER_EMAIL = $WRONG_EMAIL ]
then
    export GIT_COMMITTER_EMAIL=$NEW_EMAIL
fi
if [ \$GIT_AUTHOR_EMAIL = $WRONG_EMAIL ]
then
    export GIT_AUTHOR_EMAIL=$NEW_EMAIL
fi
" --tag-name-filter cat -- --branches --tags

# git filter-branch -f --env-filter 'export GIT_AUTHOR_EMAIL="victor@ge.com" GIT_AUTHOR_NAME="Victor"' origin/master..tmp
