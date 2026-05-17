#!/bin/bash -x

set -euo pipefail

RECIPIENT_NAME=${1} && shift
SENDER_NAME=${SENDER_NAME:-ealfonso}

RECIPIENT="${ADDRESSES_DIR}/${RECIPIENT_NAME}"
SENDER="${ADDRESSES_DIR}/${SENDER_NAME}"

vi "${RECIPIENT}"

print-10-envelope.sh -r "${RECIPIENT}" -s "${SENDER}" -1
