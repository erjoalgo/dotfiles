#!/bin/bash -x

set -euo pipefail

cd $( dirname "${BASH_SOURCE[0]}" )

./installs/install-stumpwm.sh
./installs/emacs-install.sh
./installs/roxterm-install.sh
sudo apt-get install -y chromium zathura

# TODO wifi-boot
# TODO automate debian installer seed
# TODO automate vimium installation
