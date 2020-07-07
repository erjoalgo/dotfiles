#!/bin/bash -x

set -euo pipefail

sudo apt-get install -y python3-pyudev python3-gi gir1.2-gtk-3.0

sudo pip3 install solar
