#!/bin/bash -x

set -euo pipefail

if command -v lein && lein version; then
    exit 0
fi

LEIN="${HOME}/.local/bin/lein"

mkdir -p $(dirname ${LEIN})

if ! test -f ${LEIN}; then
    URL=https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein
    curl "${URL}" -o ${LEIN} || exit ${LINENO}
fi

if ! command -v java; then
    sudo apt-get install -y default-jre
fi

chmod +x ${LEIN}
${LEIN}
