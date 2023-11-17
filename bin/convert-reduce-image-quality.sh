#!/bin/bash -x

set -euo pipefail

QUALITY=${QUALITY:-15}

for INPUT in $*; do
    OUTPUT=$(sed 's/[.]/.low-quality./' <<< "${INPUT}")
    convert "${INPUT}" -quality ${QUALITY} "${OUTPUT}"
    echo "wrote to ${OUTPUT} using quality ${QUALITY}"
done
