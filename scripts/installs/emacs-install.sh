#!/bin/bash -x

set -euo pipefail

# URL=http://gnu.mirrors.pair.com/gnu/emacs/emacs-25.2.tar.gz
# URL=http://gnu.mirrors.hoobly.com/emacs/emacs-25.2.tar.gz
# URL=http://ftp.gnu.org/gnu/emacs/emacs-25.3.tar.xz

EMACS_VERSION=27.2
URL=https://ftp.gnu.org/gnu/emacs/emacs-${EMACS_VERSION}.tar.xz
EXT=$(grep -o "[.]tar..z$" <<< "${URL}")

if false && emacs --version | grep -F "${EMACSS_VERSION}"; then
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
    sudo apt-get install -y aspell-es emacs-goodies-el
    sudo apt-get install -y uuid-runtime || true
    # gpg --keyserver hkps://keyserver.ubuntu.com --recv-key 91C1262F01EB8D39
    curl -sSL https://www.gnutls.org/pgpkey-nmav.txt | gpg --import -
    sudo apt-get install -y libgmp-dev
    sudo apt-get install -y gnutls-dev libncurses5-dev
    sudo apt-get install -y uuid-runtime

    # install-from-source -u "https://www.gnupg.org/ftp/gcrypt/gnutls/v3.6/gnutls-3.6.5.tar.xz"
fi

install-from-source -u ${URL} -g 7C207910
cd /usr/local/src/emacs-${EMACS_VERSION}
sudo make && sudo make install
