#!/bin/bash -x

set -euo pipefail

cd $( dirname "${BASH_SOURCE[0]}" )

sudo apt-get install -y dirmngr || true

./installs/install-stumpwm.sh
./installs/emacs-install.sh
./installs/roxterm-install.sh || true
sudo apt-get install -y chromium zathura
sudo apt-get install -y eog

sudo $(which insert-text-block) '# 37561c4f-5b87-4252-9724-6eed90ee3943-fix-stretch-X-issue'  \
                  /etc/X11/Xwrapper.config<<EOF
needs_root_rights=yes
EOF

# TODO wifi-boot
# TODO automate debian installer seed
# TODO automate vimium installation
