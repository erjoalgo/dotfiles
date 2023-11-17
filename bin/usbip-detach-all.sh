#!/bin/bash -x

set -euo pipefail

for PORT in $(sudo usbip port | grep -Po '(?<=^Port )[0-9]+'); do
  sudo usbip detach -p ${PORT} || true
done
