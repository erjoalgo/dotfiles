#!/bin/bash

set -euo pipefail

while getopts "t:h" OPT; do
    case ${OPT} in
    t)
        TEL=${OPTARG}
        ;;
    s)
        SIP_HOST=${OPTARG}
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))

SIP_HOST=${SIP_HOST:-$(grep -Po '(?<=^domain=).*$' ${HOME}/.linphonerc | head -1)}
TEL=$(sed 's/^tel://'  <<< "${TEL}")

set +e
ERROR=$(linphonecsh dial ${TEL}@${SIP_HOST} 2>&1)
RET=$?
set -e

if test ${RET} -ne 0; then
    xmessage ${ERROR} -timeout 2
fi


