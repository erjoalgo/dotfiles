#!/bin/bash -x

set -euo pipefail

# URL=http://gnu.mirrors.pair.com/gnu/emacs/emacs-25.2.tar.gz
# URL=http://gnu.mirrors.hoobly.com/emacs/emacs-25.2.tar.gz
# URL=http://ftp.gnu.org/gnu/emacs/emacs-25.3.tar.xz

URL=https://ftp.gnu.org/gnu/emacs/emacs-26.1.tar.xz
EXT=$(grep -o "[.]tar..z$" <<< "${URL}")

VERSION=$(grep -Po '(?<=emacs-)[0-9]+[.][0-9]+' <<< "${URL}")
if false && emacs --version | grep -F "${VERSION}"; then
    echo emacs ${VERSION} already installed
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
elif command -v apt-get; then
    sudo apt-get build-dep -y emacs
    sudo apt-get install -y aspell-es emacs-goodies-el
    sudo apt-get install -y uuid-runtime || true
    curl -sSL https://www.gnutls.org/pgpkey-nmav.txt | gpg --import -

    sudo apt-get install -y libgmp-dev

    install-from-source -u https://ftp.gnu.org/gnu/nettle/nettle-3.4.tar.gz \ -i 28C67298

    install-from-source -u "https://www.gnupg.org/ftp/gcrypt/gnutls/v3.6/gnutls-3.6.5.tar.xz"
fi

install-from-source -u ${URL} -g 7C207910
