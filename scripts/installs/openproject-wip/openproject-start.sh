#!/bin/bash -x

set -euo pipefail

OPENPROJECT_SRC=${OPENPROJECT_SRC:-${HOME}/git/openproject}
cd "${OPENPROJECT_SRC}"
# https://github.com/nodenv/nodenv/issues/170
eval "$(nodenv init -)"

# https://github.com/rbenv/rbenv/issues/1300
eval "$(rbenv init -)"

foreman start -f Procfile.dev
