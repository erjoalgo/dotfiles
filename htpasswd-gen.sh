#!/bin/bash

set -euo pipefail

USAGE=false
HTPASSWD_FILE=""

while getopts "hf:" OPT; do
    case ${OPT} in
    f)
        HTPASSWD_FILE=${OPTARG}
        ;;
    h)
        USAGE=true
        ;;
    esac
done

if test ${USAGE} = true -o -z "${HTPASSWD_FILE}"; then
    echo "usage: $(basename ${0}) -f HTPASSWD_FILE [ USER...  ]"
    exit 1
fi

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
