#!/bin/bash -x

set -euo pipefail

# POLICIES_DIR=$(ls /etc/*chrom* /etc/opt/*chrom*)/managed
POLICIES_DIR=/etc/chromium/

sudo mkdir -p ${POLICIES_DIR}/{managed,recommended}
echo '{"ExternalProtocolDialogShowAlwaysOpenCheckbox": true}' |  \
    sudo tee ${POLICIES_DIR}/managed/protocol_open.json > /dev/null
