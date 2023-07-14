#!/bin/bash -x

set -euo pipefail

TARGET=${1} && shift

sudo lsof -D "${TARGET}"
sudo fuser -vm "${TARGET}"
sudo losetup -la
