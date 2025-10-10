#!/bin/bash

set -euo pipefail

FILE=${1} && shift
pdfinfo "${FILE}" | grep -Po "(?<=Pages:).*" | tr -d ' '
# for FILE in ${*}; do done
