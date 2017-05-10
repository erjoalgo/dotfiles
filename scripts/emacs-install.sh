#!/bin/bash -x

URL=http://gnu.mirrors.pair.com/gnu/emacs/emacs-25.2.tar.gz

FNAME=$(basename ${URL})
cd ~/Downloads

if ! test -f ${FNAME}; then
    wget ${URL} || exit ${LINENO}
fi

if ! test -f ${FNAME}.sig; then
    wget ${URL}.sig || exit ${LINENO}
fi

# gpg --recv-keys BE216115
gpg --recv-keys 7C207910 || exit ${LINENO}
gpg --verify ${FNAME}{.sig,} || exit ${LINENO}

INSTALL_DIR=/usr/local
DNAME=$(basename ${FNAME} .tar.gz)

if ! test -d ${INSTALL_DIR}/${DNAME}; then
    sudo tar -C ${INSTALL_DIR} -xvf ${DNAME} || exit ${LINENO}
fi
cd ${INSTALL_DIR}/${DNAME} || exit ${LINENO}


if command -v yum; then
    sudo yum install -y yum-utils rpm-build
    yumdownloader --source emacs
    # yum install -y libX11-devel
    # sudo yum -y groupinstall "Additional Development" || exit ${LINENO}
    # rpmbuild --rebuild ftp://rpmfind.net/linux/fedora/linux/development/rawhide/Everything/source/tree/Packages/e/emacs-25.2-2.fc27.src.rpm
    sudo yum install -y giflib-devel libXdmcp-devel m17n-lib-devel  \
    libotf-devel ImageMagick-devel GConf2-devel gpm-devel  \
    liblockfile-devel webkitgtk4-devel python3-devel 
fi

./configure && make && sudo make install || exit ${LINENO}
