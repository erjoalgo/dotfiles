#!/bin/bash -x

set -euo pipefail
HTPASSWD_FILE=${1} && shift
LEN=8
test -e ${HTPASSWD_FILE} || touch ${HTPASSWD_FILE}
for USER in $*; do
    PASS=$(genpasswd ${LEN})
    echo "adding ${USER} => ${PASS}"
    htpasswd -b ${HTPASSWD_FILE} ${USER} ${PASS}
done
