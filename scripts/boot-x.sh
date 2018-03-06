#!/bin/bash -x

set -euo pipefail

cd $( dirname "${BASH_SOURCE[0]}" )

sudo apt-get install -y dirmngr || true

./installs/install-stumpwm.sh
./installs/emacs-install.sh
./installs/roxterm-install.sh || true
sudo apt-get install -y chromium zathura
sudo apt-get install -y eog


# TODO wifi-boot
# TODO automate debian installer seed
# TODO automate vimium installation
