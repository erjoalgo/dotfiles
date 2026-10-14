#!/bin/bash

# Tests for ir.sh: refuses to install a unit when irremote is missing.

set -euo pipefail

DIR=$(mktemp -d)
trap 'rm -r "${DIR}"' EXIT
INSTALL="$(realpath "$(dirname "$0")")/ir.sh"
UNIT=${DIR}/unit

mkdir "${DIR}/bin"
cat > "${DIR}/bin/install-systemd-service.sh" <<STUB
#!/bin/bash
cat > "${UNIT}"
STUB
chmod +x "${DIR}/bin/install-systemd-service.sh"

fail() { echo "FAIL: $*"; cat "${UNIT}" 2>/dev/null; exit 1; }

if PATH=${DIR}/bin:/usr/bin:/bin "${INSTALL}" 2>/dev/null; then
  fail "succeeded without irremote on PATH"
fi
[ ! -e "${UNIT}" ] || fail "unit written without irremote on PATH"

printf '#!/bin/sh\n' > "${DIR}/bin/irremote"
chmod +x "${DIR}/bin/irremote"
PATH=${DIR}/bin:/usr/bin:/bin "${INSTALL}" 2>/dev/null
grep -qx "ExecStart=${DIR}/bin/irremote  --port 2727" "${UNIT}" \
  || fail "ExecStart does not point at irremote"

echo PASS
