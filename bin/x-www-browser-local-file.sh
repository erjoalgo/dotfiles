#!/bin/bash -x

set -euo pipefail

FILENAME=${1} && shift

URL="file://${FILENAME}"

x-www-browser "${URL}"
