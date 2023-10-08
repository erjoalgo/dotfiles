#!/bin/bash -x

set -euo pipefail

x-service-curl /run -d "only"
x-service-curl /run -d "fclear"
sleep 1 && x-service-curl /run -d "vsplit" &
xev | grep --line-buffered -P 'keycode|^Key'
