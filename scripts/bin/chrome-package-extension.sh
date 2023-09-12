#!/bin/bash -x

set -euo pipefail

sudo apt-get install -y jq

EXTENSION_TOP=$(realpath ${1:-.})
test -d ${EXTENSION_TOP}

cd ${EXTENSION_TOP}/..
BASE=$(basename "${EXTENSION_TOP}")
test -d ${BASE}

VERSION=$(jq .version ${EXTENSION_TOP}/src/manifest.json | tr -d '"')

rm ${BASE}*.zip || true
ZIP=${BASE}-$(date -I)-v${VERSION}.zip
cd ${BASE}
zip -r ${ZIP} *
mv ${ZIP} ..
