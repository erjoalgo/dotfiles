#!/bin/bash -x

set -euo pipefail

cd $( dirname "${BASH_SOURCE[0]}" )

sudo apt-get install -y dirmngr || true

./installs/install-stumpwm.sh
if ! emacs --version | grep "26\\|27"; then
  if ! ./installs/emacs-install.sh; then
    echo "warning: failed to build emacs from source"
    which emacs
  fi
fi

sudo apt-get install -y zathura konsole pass keynav
sudo apt-get install -y eog scrot
test -n "$(which google-chrome)" || sudo apt-get install -y chromium

sudo DEBIAN_FRONTEND=noninteractive apt-get install -y wireless-tools wpasupplicant \
  macchanger expect iw net-tools
sudo apt-get install -y libxcomposite-dev

./installs/install-xsecurelock.sh

./installs/install-chrome-extensions.sh < ../data/public/chrome-extension-urls.txt

# https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=856351
sudo insert-text-block '# 37561c4f-5b87-4252-9724-6eed90ee3943-fix-stretch-X-issue'  \
                  /etc/X11/Xwrapper.config<<EOF
needs_root_rights=yes
EOF

which update-config-file-key-value

sudo $(which update-config-file-key-value) \
  -f /etc/systemd/logind.conf  \
  -k HandlePowerKey -v ignore

sudo service systemd-logind restart

# https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=851810
# fix xcalib unsupported ramp size
sudo insert-text-block \
     '# ed74222e-d8c2-4920-a450-fa17d93e7650-fix-xcalib-broken'  \
     --parents /etc/X11/xorg.conf.d/20-intel.conf<<EOF
Section "Device"
  Identifier "Intel Graphics"
  Driver "intel"
EndSection
EOF


insert-text-block '# eab944d5-9973-4f44-b2e0-1b168f164397-konsolerc-defaults'  \
                  ${HOME}/.config/konsolerc << EOF
# konsolerc file tends to be managed by konsole in a site-specific way.
[Desktop Entry]
DefaultProfile=erjoalgo.profile

[Favorite Profiles]
Favorites=

[KonsoleWindow]
ShowMenuBarByDefault=false
EOF

# TODO wifi-boot
# TODO automate vimium installation
echo success
