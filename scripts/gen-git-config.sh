#!/bin/bash -x

set -euo pipefail

GITCONFIG=${HOME}/.gitconfig

if ! test -e ${GITCONFIG}; then
    cat << EOF > ${GITCONFIG}
[user]
	email = ${USER}@$(hostname)
	name = $(getent passwd $USER | cut -d: -f 5)

[color]
  diff = auto
  status = auto
  branch = auto
  interactive = auto
  ui = true
  pager = true

[branch]
  autosetupmerge = always

EOF
fi
