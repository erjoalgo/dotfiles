#!/bin/bash -x

set -euo pipefail

if ! command -v tesseract; then
    sudo apt-get install -y tesseract
fi

tesseract ${*}
