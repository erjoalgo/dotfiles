#!/bin/bash -x

set -euo pipefail

CERT=${1} && shift
ALIAS=${1} && shift
KEYSTORE=${KEYSTORE:-"$JAVA_HOME/jre/lib/security/cacerts"}
STOREPASS=${STOREPASS:-changeit}

which keytool
test -e ${CERT}
test -n "${ALIAS}"

sudo $(which keytool) -import -alias ${ALIAS} \
     -file ${CERT} -keystore ${KEYSTORE} \
     -storepass ${STOREPASS} -noprompt
