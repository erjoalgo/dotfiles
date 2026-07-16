#!/bin/bash

set -euo pipefail

SOCKET="/run/user/${UID}/timestampfixer.sock"
RESP=$(curl -s --unix-socket "${SOCKET}" http://localhost)

if test "${RESP}" = "None" > /dev/null; then
    echo "no last filename found"
    exit ${LINENO}
fi

echo "${RESP}"

