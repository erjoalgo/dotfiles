#!/bin/bash

set -euo pipefail

if ! command -v tesseract > /dev/null; then
    sudo apt-get install -y tesseract
fi

tesseract ${*}
