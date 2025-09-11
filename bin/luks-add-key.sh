#!/bin/bash -x

set -euo pipefail

sudo cryptsetup luksAddKey /dev/sda3
