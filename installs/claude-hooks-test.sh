#!/bin/bash

# Tests for claude-hooks.sh: preserves site-local settings, is idempotent.

set -euo pipefail

DIR=$(mktemp -d)
trap 'rm -r "${DIR}"' EXIT
INSTALL="$(dirname "$0")/claude-hooks.sh"
S=${DIR}/settings.json

cat > "${S}" <<EOF
{"theme": "auto",
 "hooks": {"Stop": [{"hooks": [{"type": "command", "command": "site-local-cmd"}]}]}}
EOF

"${INSTALL}" "${S}" 2>/dev/null
"${INSTALL}" "${S}" 2>/dev/null

fail() { echo "FAIL: $*"; cat "${S}"; exit 1; }
count() { jq "[.hooks.$1[].hooks[] | select(.command | test(\"claude-notify-hook\"))] | length" "${S}"; }

[ "$(jq -r .theme "${S}")" = auto ] || fail "top-level setting lost"
jq -e '.hooks.Stop[].hooks[] | select(.command == "site-local-cmd")' "${S}" >/dev/null \
  || fail "site-local hook lost"
for e in UserPromptSubmit Notification Stop; do
  [ "$(count $e)" = 1 ] || fail "$e: expected exactly one claude-notify-hook entry"
done

rm "${S}"
"${INSTALL}" "${S}" 2>/dev/null
[ "$(count Stop)" = 1 ] || fail "missing settings file not created"

echo PASS
