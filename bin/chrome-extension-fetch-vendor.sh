#!/bin/bash

set -euo pipefail

# Download the third-party files listed in EXTENSION_DIR/vendor.txt, as
# "URL SHA256" lines, into EXTENSION_DIR. Files already present with the
# right hash are left alone.

cd "${1:?usage: $0 EXTENSION_DIR}"

grep -v -e '^#' -e '^\s*$' vendor.txt | while read -r URL SHA256; do
    FILE=$(basename "${URL}")
    if sha256sum --check --status <<< "${SHA256}  ${FILE}" 2>/dev/null; then
        continue
    fi
    curl -fsSL -o "${FILE}.tmp" "${URL}"
    if ! sha256sum --check --status <<< "${SHA256}  ${FILE}.tmp"; then
        rm "${FILE}.tmp"
        echo "sha256 mismatch for ${URL}" >&2
        exit 1
    fi
    mv "${FILE}.tmp" "${FILE}"
done
