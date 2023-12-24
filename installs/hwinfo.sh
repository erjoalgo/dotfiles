#!/bin/bash -x

set -euo pipefail

sudo apt-get install -y inxi
sudo apt-get install -y lshw pciutils usbutils procinfo util-linux
