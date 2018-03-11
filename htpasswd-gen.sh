#!/bin/bash -x

set -euo pipefail
function genpasswd {
    local l=$1;
    [ "$l" == "" ] && l=16;
    tr -dc A-Za-z0-9_ < /dev/urandom | head -c ${l} | xargs;
}

HTPASSWD_FILE=${1} && shift
LEN=8
test -e ${HTPASSWD_FILE} || touch ${HTPASSWD_FILE}
for USER in $*; do
    PASS=$(genpasswd ${LEN})
    echo "adding ${USER} => ${PASS}"
    htpasswd -b ${HTPASSWD_FILE} ${USER} ${PASS}
done
