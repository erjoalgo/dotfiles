#!/bin/bash -x

set -euo pipefail

# POLICIES_DIR=$(ls /etc/*chrom* /etc/opt/*chrom*)/managed


for POLICIES_DIR in /etc/opt/chrome/policies/ /etc/chromium/policies; do
    sudo mkdir -p ${POLICIES_DIR}/{managed,recommended}
    echo '{"ExternalProtocolDialogShowAlwaysOpenCheckbox": true}' |  \
        sudo tee ${POLICIES_DIR}/managed/protocol_open.json > /dev/null

    cat <<EOF | sudo tee ${POLICIES_DIR}/managed/allow_tel_protocol.json > /dev/null
{
  "URLAllowlist": [
    "tel:*"
  ]
}
EOF

done
