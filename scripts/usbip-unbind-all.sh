#!/bin/bash -x

set -euo pipefail

for DEVICE in $(sudo usbip list -pl | grep -Po '(?<=^busid=)[^#]+'); do
  sudo usbip unbind -b ${DEVICE} || true
done
