#!/bin/bash -x

set -euo pipefail

TARGET=${1} && shift

sudo lsof "${TARGET}" || true
sudo fuser -vm "${TARGET}"
sudo losetup -la
