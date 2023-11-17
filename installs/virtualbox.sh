#!/bin/bash -x

set -euo pipefail

DEB_URL=https://download.virtualbox.org/virtualbox/7.0.12/virtualbox-7.0_7.0.12-159484~Debian~bookworm_amd64.deb
cd $(mktemp -d)
wget "${DEB_URL}"
sudo dpkg -i *deb

vboxmanage setextradata global GUI/SuppressMessages "all"
