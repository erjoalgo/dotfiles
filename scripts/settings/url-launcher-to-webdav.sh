#!/bin/bash
set -euo pipefail

DATAFN=${1:-url-launcher-data} && shift || true
PARENT=${1:-webdav} && shift || true

mkdir -p ${PARENT}
#  https://unix.stackexchange.com/questions/7011/
while IFS= read -r LINE; do
    KEY=$(cut -f1 <<< "${LINE}")
    URL=$(cut -f2 <<< "${LINE}")
    ! grep "/" <<< "${KEY}"
    OUT="${PARENT}/${KEY}"
    if test -e "${OUT}"; then
	CONTENTS=$(cat "${OUT}")
	if test "$URL" != "${CONTENTS}"; then
	    echo "warning: not overwriting $KEY"
	else
	    true
	fi
    else
	! test "${DATAFN}" != "${KEY}"
	echo "${URL}" > "${OUT}"
    fi
done < "${DATAFN}"
