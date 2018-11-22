#!/bin/bash -x

set -euo pipefail

EXTENSION_TOP=$(realpath ${1}) && shift
test -d ${EXTENSION_TOP}

cd ${EXTENSION_TOP}/..
BASE=$(basename "${EXTENSION_TOP}")
test -d ${BASE}


rm ${BASE}*.zip || true
ZIP=${BASE}-$(date -I).zip
cd ${BASE}
zip -r ${ZIP} *
mv ${ZIP} ..
