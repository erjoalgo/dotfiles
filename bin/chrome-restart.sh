#!/bin/bash -x

set -euo pipefail

pgrep -af chrom

SELF_PID=$$

while PIDS=$(pgrep -A -af chrom |  grep -v ${SELF_PID}); do
    cut -f1 -d' ' <<< "${PIDS}" | xargs -L1 kill
    sleep 1;
done

chrome.sh
