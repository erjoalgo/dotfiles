#!/bin/bash

set -uo pipefail

OUTPUT=$(linphone-dial.sh ${*} 2>&1)

if test $? != 0 ; then
    echo ${OUTPUT}
    xmessage "error: ${OUTPUT}" -timeout 9999 || true
fi
