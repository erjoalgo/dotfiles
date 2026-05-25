#!/bin/bash -x

set -euo pipefail

if ! command -v twine; then
    pip install twine
fi

command -v twine

pip install build

python -m build

twine check dist/*

gpg --detach-sign -a dist/*

CMD=(twine upload --verbose dist/*)

read -p "${CMD} :"
${CMD[@]}
