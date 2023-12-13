#!/bin/bash -x

set -euo pipefail

URL=https://ftp.gnu.org/gnu/emacs/emacs-29.1.tar.xz

EMACS_VERSION=$(grep -Po '(?<=emacs-)[0-9]+[.][0-9]+' <<< "${URL}")
EXT=$(grep -o "[.]tar..z$" <<< "${URL}")

if command -v emacs && emacs --version | grep -F "${EMACSS_VERSION}"; then
    echo emacs ${EMACS_VERSION} already installed
    exit 0
fi

if command -v yum; then
    sudo yum install -y yum-utils rpm-build
    yumdownloader --source emacs
    # yum install -y libX11-devel
    # sudo yum -y groupinstall "Additional Development"
    # rpmbuild --rebuild ftp://rpmfind.net/linux/fedora/linux/development/rawhide/Everything/source/tree/Packages/e/emacs-25.2-2.fc27.src.rpm
    sudo yum install -y giflib-devel libXdmcp-devel m17n-lib-devel  \
    libotf-devel ImageMagick-devel GConf2-devel gpm-devel  \
    liblockfile-devel webkitgtk4-devel python3-devel

    sudo yum install -y libtool libtool-ltdl libtool-ltdl-devel

    sudo yum install -y libXpm libXpm-devel.x86_64
    sudo yum install -y gnutls gnutls-utils
    sudo yum install -y hunspell{,-es}
    sudo yum install -y uuid-runtime
elif command -v apt-get; then
    sudo apt-get build-dep -y emacs
    sudo apt-get install -y aspell-es emacs-goodies-el uuid-runtime  \
         libgmp-dev gnutls-dev libncurses5-dev uuid-runtime
fi

install-from-source -u ${URL} -g 7C207910
cd /usr/local/src/emacs-${EMACS_VERSION}
sudo make && sudo make install
