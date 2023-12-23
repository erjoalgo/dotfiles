#!/bin/bash -x

set -euo pipefail

# from https://serverfault.com/a/146581/170017
# and related comment
XAUTH=$(ps wwwwaux | grep -Po '(?<=[-]auth )[^ ]+')
while true; do
    x11vnc -xauth "${XAUTH}" -display :0 -shared -noxrecord -noxfixes -noxkb
    sleep 1
done
