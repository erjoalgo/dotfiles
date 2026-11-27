#!/bin/bash
# Recreate a venv on the current python3 and reinstall its top-level
# packages: editables from the same path, the rest by name at their latest
# version, since the old pins may not support the new python.

set -euo pipefail

VENV=${1:-${HOME}/.venv}

if ! test -f "${VENV}/pyvenv.cfg"; then
    echo "${VENV} is not a venv (no pyvenv.cfg)" >&2
    exit 1
fi

# Ask the system pip what to reinstall: it reads the old site-packages'
# metadata without importing anything, so it works on a venv whose python
# is gone. --not-required keeps the top of the dependency tree; installing
# those pulls in the rest. Editables come back editable from their path.
PATHS=()
for SITE in "${VENV}"/lib/python3*/site-packages; do
    PATHS+=(--path "${SITE}")
done
LIST=(/usr/bin/python3 -m pip list "${PATHS[@]}" --not-required)
mapfile -t EDITABLES < <("${LIST[@]}" --editable --format=json |
                             grep -oP '"editable_project_location": "\K[^"]+')
mapfile -t PACKAGES < <("${LIST[@]}" --exclude-editable --format=freeze \
                            --exclude pip --exclude setuptools --exclude wheel |
                            sed 's/==.*//')
SPECS=("${PACKAGES[@]}")
for EDITABLE in "${EDITABLES[@]}"; do
    SPECS+=("-e ${EDITABLE}")
done

echo "will recreate ${VENV} on $(python3 --version) and install:"
if test ${#SPECS[@]} -gt 0; then
    printf '    %s\n' "${SPECS[@]}"
else
    echo "    (none)"
fi
read -r -p "go ahead? [y/N] " ANSWER < /dev/tty
if test "${ANSWER,,}" != y; then
    echo "nothing changed"
    exit 1
fi

python3 -m venv --clear "${VENV}"

# one at a time, so a package that fails on the new python doesn't stop the rest
FAILED=()
for SPEC in "${SPECS[@]}"; do
    if test "${SPEC:0:3}" = "-e "; then
        "${VENV}/bin/pip" install -e "${SPEC:3}" || FAILED+=("${SPEC}")
    else
        "${VENV}/bin/pip" install "${SPEC}" || FAILED+=("${SPEC}")
    fi
done

if test ${#FAILED[@]} -gt 0; then
    echo "failed to install:" >&2
    printf '    %s\n' "${FAILED[@]}" >&2
    exit 1
fi
echo "rebuilt ${VENV}"
