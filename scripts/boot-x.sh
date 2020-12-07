#!/bin/bash -x

set -euo pipefail

cd $( dirname "${BASH_SOURCE[0]}" )

sudo apt-get install -y dirmngr || true

./installs/install-stumpwm.sh
if ! emacs --version | grep "26\\|27"; then
  if ! ./installs/emacs-install.sh; then
    echo "warning: failed to build emacs from source"
    which emacs || true
  fi
fi

sudo apt-get install -y zathura konsole pass keynav at
sudo apt-get install -y eog scrot
test -n "$(which google-chrome)" || sudo apt-get install -y chromium

sudo DEBIAN_FRONTEND=noninteractive apt-get install -y wireless-tools wpasupplicant \
  macchanger expect iw net-tools
sudo apt-get install -y libxcomposite-dev

./installs/install-xsecurelock.sh

./installs/install-chrome-extensions.sh < ../data/public/chrome-extension-urls.txt

./installs/chrome-disable-xdg-open-prompt.sh

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

KONSOLERC=${HOME}/.config/konsolerc
if test -e "$KONSOLERC"; then
    sed -i '/^DefaultProfile=/d' ${KONSOLERC}
fi

insert-text-block '# eab944d5-9973-4f44-b2e0-1b168f164397-konsolerc-defaults'  \
                  ${KONSOLERC} -b << EOF
# konsolerc file tends to be managed by konsole in a site-specific way.
[Desktop Entry]
DefaultProfile=erjoalgo.profile

[Favorite Profiles]
Favorites=

[KonsoleWindow]
ShowMenuBarByDefault=false
EOF


sudo insert-text-block '# Zss7UaEgcFtP1T8JPS7h77vOaQlYDR3H-enable-autologin' \
     /etc/systemd/logind.conf<<EOF
NAutoVTs=1
EOF

AUTOLOGIN_CONF=/etc/systemd/system/getty@tty1.service.d/autologin.conf
# autologin to stumpwm on tty1
sudo mkdir -p $(dirname "${AUTOLOGIN_CONF}")

sudo insert-text-block '# e8a6c230-997f-4dd5-9b57-7e3b31ab67bc'  \
     "${AUTOLOGIN_CONF}" <<EOF
[Service]
ExecStart=-/sbin/agetty --autologin "${USER}" %I
EOF

sudo systemctl enable getty@tty1.service


for SYSTEM in ../lisp/{statusor,cladaver}; do
  SYSTEM=$(realpath ${SYSTEM})
  asdf-add-project-to-link-farm ${SYSTEM}
  asdf-system-installed-p $(basename ${SYSTEM}/*asd .asd)
  ln -sf ${SYSTEM} ~/quicklisp/local-projects
done

# set up xdg-open configs
for XDG_OPEN_SCRIPT in ./installs/xdg-open-*; do
    ${XDG_OPEN_SCRIPT} || true
done

python3 -m pip install pyudev
# for the logitech wireless keyboard
sudo python3 -m pip install solaar

install-systemd-service.sh pyudevs <<EOF
[Unit]
Description=Run custom udev scripts via pyudev

[Service]
ExecStart=$(pwd)/bin/pyudevs.py
User=$(whoami)
Restart=on-failure
RestartSec=5s

EOF

# TODO wifi-boot
# TODO automate vimium installation
echo success
