#!/bin/bash -x

set -euo pipefail

DATAFN=${1:-url-launcher-data} && shift || true

PARENT=webdav
mkdir -p ${PARENT}
#  https://unix.stackexchange.com/questions/7011/
while IFS= read -r LINE; do
    KEY=$(cut -f1 <<< "${LINE}")
    URL=$(cut -f2 <<< "${LINE}")
    ! grep "/" <<< "${KEY}"
    OUT="${PARENT}/${KEY}"
    if test -e "${OUT}"; then
	if test "$URL" != $(cat $OUT); then
	    echo "warning: not overwriting $KEY"
	else
	    true
	fi
    else
	! test "${DATAFN}" != "${KEY}"
	echo "${URL}" > "${OUT}"
    fi
done < "${DATAFN}"
