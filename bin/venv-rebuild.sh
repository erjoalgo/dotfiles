#!/bin/bash
# Recreate a venv on the current python3 and reinstall the packages that
# were installed into it explicitly (pip marks those with a REQUESTED file).
# Works after its old python is gone: it only reads the old dist-info dirs.
# Editable installs are reinstalled editable from the same path; the rest
# at their latest version, since the old pins may not support the new python.

set -euo pipefail

VENV=${1:-${HOME}/.venv}

if ! test -f "${VENV}/pyvenv.cfg"; then
    echo "${VENV} is not a venv (no pyvenv.cfg)" >&2
    exit 1
fi

# collect one install spec per explicitly installed package
declare -A SPECS=()
for DIST in "${VENV}"/lib/python3*/site-packages/*.dist-info; do
    test -f "${DIST}/REQUESTED" || continue
    NAME=$(grep -m1 -oP '^Name: \K\S+' "${DIST}/METADATA")
    case "${NAME,,}" in
        pip|setuptools|wheel) continue ;;
    esac
    SPEC=${NAME}
    DIRECT_URL=${DIST}/direct_url.json
    if test -f "${DIRECT_URL}"; then
        URL=$(grep -oP '"url":\s*"\K[^"]+' "${DIRECT_URL}")
        if grep -qP '"editable":\s*true' "${DIRECT_URL}"; then
            SPEC="-e ${URL#file://}"
        elif grep -qP '"vcs":\s*"git"' "${DIRECT_URL}"; then
            SPEC="${NAME} @ git+${URL}"
        else
            SPEC="${NAME} @ ${URL}"
        fi
    fi
    SPECS[${NAME,,}]=${SPEC}
done

echo "will recreate ${VENV} on $(python3 --version) and install:"
test ${#SPECS[@]} -gt 0 || echo "    (none)"
for KEY in $(printf '%s\n' "${!SPECS[@]}" | sort); do
    echo "    ${SPECS[${KEY}]}"
done
read -r -p "go ahead? [y/N] " ANSWER < /dev/tty
if test "${ANSWER,,}" != y; then
    echo "nothing changed"
    exit 1
fi

python3 -m venv --clear "${VENV}"

# one at a time, so a package that fails on the new python doesn't stop the rest
FAILED=()
for KEY in $(printf '%s\n' "${!SPECS[@]}" | sort); do
    SPEC=${SPECS[${KEY}]}
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
