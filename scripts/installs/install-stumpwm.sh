#!/bin/bash -x

set -euo pipefail

cd "$( dirname "${BASH_SOURCE[0]}" )"

APT_GET="apt-get"
if command -v yum; then
    APT_GET="yum"
fi

sudo ${APT_GET} install -y curl autoconf make texinfo  \
     rlwrap install-info espeak

sudo ${APT_GET} install -y xinit x11-xserver-utils \
     xbacklight xcalib xsel upower xscreensaver

SBCLRC="${HOME}/.sbclrc"
if ! sbcl --eval '(ql:system-apropos :vecto)' --quit; then
    ./sbcl.sh
fi

sbcl --load "${SBCLRC}" --script /dev/stdin <<EOF
        (mapcar 'ql:quickload
	'(
"clx"
"cl-ppcre"
"swank"
"quicklisp-slime-helper"
"usocket"
"alexandria"
"cl-csv" ;; for contacts.csv support
"websocket-driver-client" ;; for fanout client
))
EOF

mkdir -p "${HOME}/src" && cd "${HOME}/src"

if ! test -d stumpwm; then
    git clone https://github.com/stumpwm/stumpwm.git
fi

STUMPWM="$(pwd)/stumpwm"
cd stumpwm

if ! command -v stumpwm || test -n "${FORCE:-}"; then #the executable
    ./autogen.sh
    ./configure
    make clean
    make
    test -d ~/bin || mkdir ~/bin
    sudo make install
    # sudo ln -sf "${STUMPWM}/stumpwm" /usr/local/bin
fi

if command -v yum; then
   sudo yum groupinstall -y "X Window System";
fi


sudo ${APT_GET} install -y xdotool
which nc || sudo ${APT_GET} install -y net-tools

XSESSION_ENTRY=/usr/share/xsessions/stumpwm.desktop

if test -d $(dirname "${XSESSION_ENTRY}"); then
   cat << EOF | sudo tee "${XSESSION_ENTRY}"
[Desktop Entry]
Encoding=UTF-8
Name=stumpwm
Comment=stumpwm
Exec=/usr/local/bin/stumpwm
Type=Application
EOF
fi

GDM_CONFIG_FILENAME=/var/lib/AccountsService/users/${USER}
if sudo test -e "${GDM_CONFIG_FILENAME}"; then
  sudo sed -i 's/^XSession=.*/XSession=stumpwm/g' "${GDM_CONFIG_FILENAME}"
fi


sudo mkdir -p /usr/share/xsessions

sudo insert-text-block  \
     '# TWl64wQwIBG3lCmaSHhigZLEnxfRU0Cr-add-stumpwm-xsession' \
     /usr/share/xsessions/stumpwm.desktop <<EOF
[Desktop Entry]
Exec=/usr/local/bin/stumpwm
Icon=/usr/share/icons/stumpwm-logo-stripe.png
Type=Application
DesktopNames=STUMPWM
DesktopNames=XFCE
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
