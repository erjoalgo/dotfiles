#!/bin/bash
# Recreate a venv on the system python3 and reinstall its top-level
# packages: editables from the same path, the rest by name at their latest
# version, since the old pins may not support the new python.

set -euo pipefail

VENV=${1:-${HOME}/.venv}

if ! test -f "${VENV}/pyvenv.cfg"; then
    echo "${VENV} is not a venv (no pyvenv.cfg)" >&2
    exit 1
fi

# pip list --format=json on stdin -> one per line, the project location
# of each editable (editables) or the name of each other package (packages)
pip-list-select() {
    /usr/bin/python3 -c '
import json, sys
editables = sys.argv[1] == "editables"
for package in json.load(sys.stdin):
    location = package.get("editable_project_location")
    if editables and location:
        print(location)
    elif not editables and not location:
        print(package["name"])
' "$1"
}

# Ask the system pip what to reinstall: it reads the old site-packages'
# metadata without importing anything, so it works on a venv whose python
# is gone. --not-required keeps the top of the dependency tree; installing
# those pulls in the rest. Editables come back editable from their path.
PATHS=()
for SITE in "${VENV}"/lib/python3*/site-packages; do
    PATHS+=(--path "${SITE}")
done
LISTED=$(/usr/bin/python3 -m pip list "${PATHS[@]}" --not-required \
              --exclude pip --exclude setuptools --exclude wheel --format=json)
mapfile -t PACKAGES < <(pip-list-select packages <<< "${LISTED}")
mapfile -t EDITABLES < <(pip-list-select editables <<< "${LISTED}")

if test $(( ${#PACKAGES[@]} + ${#EDITABLES[@]} )) -eq 0; then
    echo "found nothing to reinstall in ${VENV}; left it as is"
    exit 0
fi

echo "will recreate ${VENV} on $(/usr/bin/python3 --version) and install:"
test ${#PACKAGES[@]} -eq 0 || printf '    %s\n' "${PACKAGES[@]}"
test ${#EDITABLES[@]} -eq 0 || printf '    -e %s\n' "${EDITABLES[@]}"
read -r -p "go ahead? [y/N] " ANSWER < /dev/tty
if test "${ANSWER,,}" != y; then
    echo "nothing changed"
    exit 1
fi

/usr/bin/python3 -m venv --clear "${VENV}"

# one at a time, so a package that fails on the new python doesn't stop the rest
FAILED=()
for PACKAGE in "${PACKAGES[@]}"; do
    "${VENV}/bin/pip" install "${PACKAGE}" || FAILED+=("${PACKAGE}")
done
for EDITABLE in "${EDITABLES[@]}"; do
    "${VENV}/bin/pip" install -e "${EDITABLE}" || FAILED+=("-e ${EDITABLE}")
done

if test ${#FAILED[@]} -gt 0; then
    echo "failed to install:" >&2
    printf '    %s\n' "${FAILED[@]}" >&2
    exit 1
fi
echo "rebuilt ${VENV}"
