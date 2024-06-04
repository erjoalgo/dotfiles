#!/bin/bash -x

set -euo pipefail

xev | grep keycode &
jobs
PID=$(jobs -p)
sleep 1;
kill ${PID}
