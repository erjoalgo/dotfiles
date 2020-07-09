#!/bin/bash -x

set -euo pipefail

sudo apt-get install -y python3-pyudev python3-gi gir1.2-gtk-3.0

GIT="${HOME}/git"
mkdir -p "${GIT}"
cd "${GIT}"
test -e Solaar || git clone https://github.com/pwr-Solaar/Solaar
cd Solaar
sudo pip3 install .
