#!/bin/bash -x

# set -euo pipefail

LOG_FILE=${1:-/tmp/echo-args.log}

exec 2>> "${LOG_FILE}"
exec 1>> "${LOG_FILE}"

echo ${*}
