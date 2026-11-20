#!/bin/bash -x

set -euo pipefail

if ! command -v twine; then
    pip install twine
fi

command -v twine

pip install build

DIST=$(mktemp -d)

python -m build --outdir "${DIST}"

twine check "${DIST}"/*

for DIST_FILE in "${DIST}"/*.whl "${DIST}"/*.tar.gz; do
    gpg --detach-sign -a "${DIST_FILE}"
done

CMD=(twine upload --verbose "${DIST}"/*)

read -p "${CMD} :"
${CMD[@]}
