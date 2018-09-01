#!/bin/bash -x

set -euo pipefail


TARGET=""
for CAND in /opt/google/chrome/extensions/ \
                /usr/share/google-chrome/extensions/ \
            ; do
    if test -e "${CAND}"; then
        TARGET="${CAND}"
        break
    fi
done

EXTENSIONID=dbepggeogbaibhgnhhndojpepiihcmeb
PRODVERSION=$(chromium --version | cut -d' ' -f2)
# UPDATE_URL="https://clients2.google.com/service/update2/crx?response=redirect&prodversion=${PRODVERSION}&x=id%3D${EXTENSIONID}%26uc"
UPDATE_URL="https://clients2.googleusercontent.com/crx/blobs/QgAAAC6zw0qH2DJtnXe8Z7rUJP3alLvzbIZmhqIgCKfgk7NlSuPP-wivoMjNv1-i4BMpEV3hEPi__LdDzWad4bAt_ekE0XFGMXb9Uyz7Nb34PQn8AMZSmuVAKYzKJJmz3MaS5eBK757PalAF-w/extension_1_63_3_0.crx"
# exit 0

if test -z "${TARGET}"; then
    echo "no suitable chrom* browser target found"
    exit ${LINENO}
else
    # VIMIUM_WEBSTORE_URL="https://chrome.google.com/webstore/detail/vimium/dbepggeogbaibhgnhhndojpepiihcmeb?hl=en"
    VIMIUM_WEBSTORE_URL="${UPDATE_URL}"
    VIMIUM_JSON_FNAME=${TARGET}/${EXTENSIONID}.json

    cat <<EOF | sudo tee "${VIMIUM_JSON_FNAME}"
{"external_update_url": "${VIMIUM_WEBSTORE_URL}"}
EOF
    sudo chmod a+r "${VIMIUM_JSON_FNAME}"
fi

echo "expecting vimium to be available at ${TARGET}"


