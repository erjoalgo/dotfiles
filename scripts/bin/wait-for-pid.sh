#!/bin/bash -x

set -euo pipefail

PID=${1} && shift

while ps -fp ${PID}; do
  sleep 1
done
