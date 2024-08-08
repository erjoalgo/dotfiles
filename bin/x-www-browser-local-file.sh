#!/bin/bash -x

set -euo pipefail

FILENAME=${1} && shift

URL="file://$(realpath ${FILENAME})"

x-www-browser "${URL}"
