#!/bin/bash

set -euo pipefail

# read from stdin
echo "reading tab-separated extension ids from stdin..."
EXT_URLS=$(cat)
CHROME_WEBSTORE_URL="https://clients2.google.com/service/update2/crx"

# TODO add macos dirs
for DIR in  \
  /opt/google/chrome/ \
  /usr/share/google-chrome/ \
  ; do
  if test -d ${DIR}; then
    echo "found chrom* directory: ${DIR}"
    TOP=${DIR}/extensions
    sudo mkdir -p ${TOP}
    for URL in ${EXT_URLS}; do
      EXTID=$(basename ${URL} | cut -d'?' -f1)
      cd ${TOP}
      FILENAME=${EXTID}.json
      if ! test -e ${FILENAME}; then
        echo "installing ${EXTID} onto $(pwd)"
        cat <<EOF | sudo tee ${FILENAME}
{
    "external_update_url": "${CHROME_WEBSTORE_URL}"
}
EOF
      else
        echo "${EXTID} already installed"
      fi
    done
  fi
done

# Local Variables:
# compile-command: "./install-chrome-extensions.sh < chrome-extensions.tsv "
# End:
