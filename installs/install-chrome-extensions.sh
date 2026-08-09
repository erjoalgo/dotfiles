#!/bin/bash

set -euo pipefail

# read from stdin
echo "reading tab-separated extension ids from stdin..."
EXT_URLS=$(cat)

POLICY_ITEMS=()

for URL in ${EXT_URLS}; do
    EXTID=$(basename "${URL}" | cut -d'?' -f1)

    POLICY_ITEMS+=("    \"${EXTID};https://clients2.google.com/service/update2/crx\"")
done

# Use printf to join array elements with a comma and an actual newline
# The sed command strips the trailing comma from the very last array item
LIST=$(printf '%s,\n' "${POLICY_ITEMS[@]}" | sed '$s/,$//')

# Write out the valid JSON config
EXT_CONFIG=/etc/chromium/policies/managed/extensions.json
sudo mkdir -p "$(dirname "${EXT_CONFIG}")"

sudo tee "${EXT_CONFIG}" <<EOF
{
  "ExtensionInstallForcelist": [
${LIST}
  ]
}
EOF

for DIR in  \
    /opt/google/chrome/extensions \
        /usr/share/google-chrome/extensions \
        /usr/share/chromium/extensions \
    ; do
    if ! test -d "${DIR}"; then
        continue
    fi
    echo "removing legacy extensions at ${DIR}"
    sudo rm ${DIR}/*json || true
    sudo rmdir "${DIR}"
done

# Local Variables:
# compile-command: "./install-chrome-extensions.sh < ../../data/public/chrome-extension-urls.txt"
# End:
