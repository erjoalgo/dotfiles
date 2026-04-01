#!/bin/bash -x

set -euo pipefail


chrome-add-policy.sh -b protocol_open.json <<EOF
{
  "ExternalProtocolDialogShowAlwaysOpenCheckbox": true
}
EOF

chrome-add-policy.sh -b allow_tel_protocol.json <<EOF
{
  "URLWhitelist": [
    "tel:*"
  ],
  "URLAllowlist": [
    "tel:*"
  ]
}
EOF
