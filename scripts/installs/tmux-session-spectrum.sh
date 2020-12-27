#!/bin/bash -x

set -euo pipefail
# forked from github.com/a-rodin
DEST="${HOME}/.tmux/plugins/tmux-session-spectrum"

if ! test -d "${DEST}"; then
    git clone \
        https://github.com/erjoalgo/tmux-session-spectrum  \
        "${DEST}"
fi
