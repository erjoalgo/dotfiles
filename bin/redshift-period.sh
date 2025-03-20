#!/bin/bash

set -euo pipefail

if OUTPUT=$(timeout 2s redshift -p 2>/dev/null); then
    grep -Po "(?<=Period: ).*" <<< "${OUTPUT}"
    exit 0
fi

HOUR=$(date +%H)
if test ${HOUR} -gt 4 -a ${HOUR} -lt 19; then
    echo Daytime
else
    echo Night
fi
