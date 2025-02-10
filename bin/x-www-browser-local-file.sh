#!/bin/bash -x

set -euo pipefail

FILENAME="$(realpath "${1}")" && shift
# ENCODED=$(printf %s "${FILENAME}" | jq -sR @uri)
ENCODED=$(sed 's/ /%20/g' <<< "${FILENAME}")
URL="file://${ENCODED}"

x-www-browser "${URL}"
