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
(mapcar 'ql:quickload '(:cl-ppcre :swank :clx))
EOF

pushd .

mkdir -p "${HOME}/src" && cd "${HOME}/src"

if ! test -d stumpwm; then
    git clone https://github.com/stumpwm/stumpwm.git
fi

STUMPWM="$(pwd)/stumpwm"
cd stumpwm

FORCE=true
if ! command -v stumpwm || test -n "${FORCE:-}"; then #the executable
    ./autogen.sh
    ./configure
    make clean
    make
    test -d ~/bin || mkdir ~/bin
    sudo make install
fi
popd

if command -v yum; then
   sudo yum groupinstall -y "X Window System";
fi


sudo ${APT_GET} install -y xdotool
which nc || sudo ${APT_GET} install -y net-tools

GDM_CONFIG_FILENAME=/var/lib/AccountsService/users/${USER}
if sudo test -e "${GDM_CONFIG_FILENAME}"; then
  sudo sed -i 's/Session=.*/Session=stumpwm/g' "${GDM_CONFIG_FILENAME}"
fi

sudo mkdir -p $(dirname "${GDM_CONFIG_FILENAME}")

sudo cat <<EOF | sudo tee "${GDM_CONFIG_FILENAME}"
[User]
Session=stumpwm
Icon=$(realpath ../data/public/.face)
SystemAccount=false

[InputSource0]
xkb=us
EOF

sudo mkdir -p /usr/share/xsessions

sudo insert-text-block  \
     '# TWl64wQwIBG3lCmaSHhigZLEnxfRU0Cr-add-stumpwm-xsession' \
     /usr/share/xsessions/stumpwm.desktop <<EOF
[Desktop Entry]
Comment=stumpwm-lightdm
DesktopNames=STUMPWM
Encoding=UTF-8
Exec=/usr/local/bin/stumpwm
Icon=$(realpath ../../data/public/stumpwm-icon.png)
Name=stumpwm-lightdm
Type=Application
EOF

if test -e ~/.dmrc; then
    sed -i 's/^Session=.*/Session=stumpwm/' ~/.dmrc
fi

if test -e /usr/share/xsessions/lightdm-xsession.desktop; then
    sudo sed -i 's/^Exec=.*/Exec=stumpwm/'  \
         /usr/share/xsessions/lightdm-xsession.desktop
fi


sudo update-alternatives --install /usr/bin/x-window-manager \
  x-window-manager $(which stumpwm) 200
sudo update-alternatives --set x-window-manager $(which stumpwm)
