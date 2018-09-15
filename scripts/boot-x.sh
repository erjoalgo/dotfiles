#!/bin/bash -x

set -euo pipefail

cd $( dirname "${BASH_SOURCE[0]}" )

sudo apt-get install -y dirmngr || true

./installs/install-stumpwm.sh
if ! ./installs/emacs-install.sh; then
  echo "warning: failed to build emacs from source"
  which emacs
fi
sudo apt-get install -y chromium zathura gnome-terminal pass keynav
sudo apt-get install -y eog shutter
sudo apt-get install -y wireless-tools wpasupplicant expect

./installs/install-xsecurelock.sh

./installs/install-chrome-extensions.sh < ../data/public/chrome-extension-urls.txt

# https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=856351
sudo $(which insert-text-block) '# 37561c4f-5b87-4252-9724-6eed90ee3943-fix-stretch-X-issue'  \
                  /etc/X11/Xwrapper.config<<EOF
needs_root_rights=yes
EOF

sudo $(which update-config-file-key-value) \
  -f /etc/systemd/logind.conf  \
  -k HandlePowerKey -v ignore

sudo service systemd-logind restart

# https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=851810
sudo $(which insert-text-block) '# ed74222e-d8c2-4920-a450-fa17d93e7650-fix-xcalib-broken'  \
                  --parents /etc/X11/xorg.conf.d/20-intel.conf<<EOF
Section "Device"
  Identifier "Intel Graphics"
  Driver "intel"
EndSection
EOF


# TODO wifi-boot
# TODO automate vimium installation
echo success
