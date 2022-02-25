#!/bin/bash -x

set -euo pipefail

cd $( dirname "${BASH_SOURCE[0]}" )

sudo apt-get install -y dirmngr || true

./installs/install-stumpwm.sh

sudo mkdir -p /usr/share/xsessions

sudo insert-text-block  \
     '# TWl64wQwIBG3lCmaSHhigZLEnxfRU0Cr-add-stumpwm-xsession' \
     /usr/share/xsessions/stumpwm.desktop <<EOF
[Desktop Entry]
Exec=/usr/local/bin/stumpwm
Icon=/usr/share/icons/stumpwm-logo-stripe.png
Type=Application
DesktopNames=STUMPWM
EOF

if test -e ~/.dmrc; then
    sed -i 's/^Session=.*/Session=stumpwm/' ~/.dmrc
fi

if test -e /usr/share/xsessions/lightdm-xsession.desktop; then
    sudo sed -i 's/^Exec=.*/Exec=stumpwm/'  \
         /usr/share/xsessions/lightdm-xsession.desktop
fi


sudo update-alternatives --install /usr/bin/x-window-manager x-window-manager $(which stumpwm) 200
sudo update-alternatives --set x-window-manager $(which stumpwm)

if ! emacs --version | grep "26\\|27"; then
  if ! ./installs/emacs-install.sh; then
    echo "warning: failed to build emacs from source"
    which emacs || true
  fi
fi

sudo apt-get install -y zathura konsole pass keynav at x2x
sudo apt-get install -y eog scrot
test -n "$(which google-chrome)" || sudo apt-get install -y chromium

sudo DEBIAN_FRONTEND=noninteractive apt-get install -y wireless-tools wpasupplicant \
  macchanger expect iw net-tools
sudo apt-get install -y libxcomposite-dev

./installs/install-xsecurelock.sh

./installs/install-chrome-extensions.sh < ../data/public/chrome-extension-urls.txt

./installs/chrome-disable-xdg-open-prompt.sh

./installs/install-find-cursor.sh ||
  echo "WARNING: failed to install find-cursor"

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
NAutoVTs=3
EOF

AUTOLOGIN_CONF=/etc/systemd/system/getty@tty1.service.d/autologin.conf
# autologin to stumpwm on tty1
sudo mkdir -p $(dirname "${AUTOLOGIN_CONF}")

sudo insert-text-block '# e8a6c230-997f-4dd5-9b57-7e3b31ab67bc'  \
     "${AUTOLOGIN_CONF}" <<EOF
[Service]
ExecStart=-/sbin/agetty --autologin "${USER}" %I
EOF

sudo insert-text-block '# bZkpjy56EU9dBbdNDTk0zmMVGWJLzK9e-ssh' \
     "${HOME}/.ssh/config" <<EOF
XAuthLocation /opt/X11/bin/xauth
EOF

sudo insert-text-block '# yTyIZrilAW59XgITTlNwLp3VdhMn9k7R-enable-sysrq' \
                  /etc/sysctl.d/99-sysctl.conf<<EOF
kernel.sysrq = 1
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

sudo apt-get install python3-pip
python3 -m pip install pyudev
# for the logitech wireless keyboard
sudo python3 -m pip install solaar

install-systemd-service.sh pyudevs <<EOF
[Unit]
Description=Run custom udev scripts via pyudev
Requires=systemd-udevd.service
After=systemd-udevd.service
StartLimitInterval=0

[Service]
ExecStart=$(pwd)/bin/pyudevs.py
User=$(whoami)
Restart=always
RestartSec=5
Environment=PATH=$PATH:$(pwd)/bin

[Install]
WantedBy=graphical.target

EOF

mkdir -p ~/pictures/auto-scrots

echo success
