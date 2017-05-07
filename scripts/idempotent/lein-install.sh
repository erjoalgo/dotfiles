#!/bin/bash

if command -v lein && lein version; then
    exit 0
fi

# LEIN=/usr/local/lein
LEIN="${HOME}/bin/lein"

if ! test -d $(dirname ${LEIN}); then
    mkdir $(dirname ${LEIN}) || exit ${LINENO}
fi

if ! test -f ${LEIN}; then
    URL=https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein
    curl "${URL}" -o ${LEIN} || exit ${LINENO}
fi

if ! command -v java; then
    sudo apt-get install -y openjdk-7-jre
fi

chmod +x ${LEIN}
${LEIN}
