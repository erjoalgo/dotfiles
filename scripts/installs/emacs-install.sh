#!/bin/bash -x

set -euo pipefail

URL=http://gnu.mirrors.pair.com/gnu/emacs/emacs-25.2.tar.gz

FNAME=$(basename ${URL})
cd ~/Downloads

test -f ${FNAME} || wget ${URL}
test -f ${FNAME}.sig || wget ${URL}.sig

# gpg --recv-keys BE216115

INSTALL_DIR=/usr/local
DNAME=$(basename ${FNAME} .tar.gz)

if ! test -d ${INSTALL_DIR}/${DNAME}; then
    gpg --recv-keys 7C207910
    gpg --verify ${FNAME}{.sig,}
    sudo tar -C ${INSTALL_DIR} -xvf ${FNAME}
fi
cd ${INSTALL_DIR}/${DNAME}

if command -v yum; then
    sudo yum install -y yum-utils rpm-build
    yumdownloader --source emacs
    # yum install -y libX11-devel
    # sudo yum -y groupinstall "Additional Development"
    # rpmbuild --rebuild ftp://rpmfind.net/linux/fedora/linux/development/rawhide/Everything/source/tree/Packages/e/emacs-25.2-2.fc27.src.rpm
    sudo yum install -y giflib-devel libXdmcp-devel m17n-lib-devel  \
    libotf-devel ImageMagick-devel GConf2-devel gpm-devel  \
    liblockfile-devel webkitgtk4-devel python3-devel
elif command -v apt-get; then
    sudo apt-get build-dep -y emacs24
fi

./configure --with-xwidgets && read -p 'continue: ' &&  \
    make &&  \
    sudo make install
