#!/bin/bash

set -euo pipefail

# read from stdin
echo "reading tab-separated extension ids from stdin..."
EXT_URLS=$(cat)
CHROME_WEBSTORE_URL="https://clients2.google.com/service/update2/crx"

# TODO add macos dirs
DIRS_FOUND=""

for DIR in  \
  /opt/google/chrome/ \
  /usr/share/google-chrome/ \
  /usr/share/chromium/ \
  ; do
    if ! test -d ${DIR}; then
        echo "not found ${DIR}"
        continue
    fi
    DIRS_FOUND+=" ${DIR}"
    echo "found chrom* directory: ${DIR}"
    test -w ${DIR} && SUDOOPT="" || SUDOOPT="sudo"
    TOP=${DIR}/extensions
    ${SUDOOPT} mkdir -p ${TOP}
    for URL in ${EXT_URLS}; do
      EXTID=$(basename ${URL} | cut -d'?' -f1)
      cd ${TOP}
      FILENAME=${EXTID}.json
      if ! test -e ${FILENAME}; then
        echo "installing ${EXTID} onto $(pwd)"
        cat <<EOF | ${SUDOOPT} tee ${FILENAME}
{
  "external_update_url": "${CHROME_WEBSTORE_URL}"
}
EOF
      else
        echo "${EXTID} already installed"
      fi
    done
done

if test -z "${DIRS_FOUND}"; then
    echo "no chrome dirs found!"
    exit ${LINENO}
else
    echo "installed extensions to the following directories: "
    echo "${DIRS_FOUND}"
fi

# Local Variables:
# compile-command: "./install-chrome-extensions.sh < ../../data/public/chrome-extension-urls.txt"
# End:
