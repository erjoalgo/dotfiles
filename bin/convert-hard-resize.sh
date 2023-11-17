#!/bin/bash -x

set -euo pipefail

INPUT=${1} && shift
DIMENSIONS=${1} && shift
OUTPUT=${1} && shift

# from https://stackoverflow.com/questions/32466048

convert "${INPUT}" -resize "${DIMENSIONS}^" -gravity Center  \
        -extent "${DIMENSIONS}" "${OUTPUT}"
