#!/bin/bash


APT_GET="apt-get"
if command -v yum; then
	APT_GET="yum"
fi

sudo ${APT_GET} install -y sbcl curl autoconf make texinfo rlwrap || exit ${LINENO}

sudo ${APT_GET} install -y xinit x11-xserver-utils \
     xbacklight xcalib xsel upower|| exit ${LINENO}

if command -v stumpwm; then
    echo "stumpwm already installed"
    exit 0
fi


SBCLRC="${HOME}/.sbclrc"
if test -f "${SBCLRC}"; then
    sbcl --load "${SBCLRC}" --script /dev/stdin <<< '(ql:quickload "cl-ppcre")'
fi

if ! test $? -eq 0; then
    curl -O http://beta.quicklisp.org/quicklisp.lisp || exit ${LINENO}
    sbcl --load quicklisp.lisp --script  \
	 /dev/stdin <<< '(quicklisp-quickstart:install) (ql:add-to-init-file)' \
	|| exit ${LINENO}
    rm quicklisp.lisp
fi

sbcl --load "${SBCLRC}" --script /dev/stdin <<EOF
(mapcar 'ql:quickload
	'(
"clx"
"cl-ppcre"
"swank"
"quicklisp-slime-helper"
"usocket"
))
EOF

PROGRAMS="${HOME}/programs"
test -d "${PROGRAMS}" || mkdir "${PROGRAMS}"
cd "${PROGRAMS}"

if ! test -d stumpwm; then
    git clone https://github.com/stumpwm/stumpwm.git || exit ${LINENO}
fi

STUMPWM="${PROGRAMS}/stumpwm"
cd stumpwm

if ! command -v stumpwm; then #the executable
    read -p "confirm make in ${PWD}: "
    ./autogen.sh || exit ${LINENO}
    ./configure || exit ${LINENO} 
    make || exit ${LINENO}
    test -d ~/bin || mkdir ~/bin
    sudo ln -s "${STUMPWM}/stumpwm" /usr/local/bin
fi

if command -v yum; then
   yum groupinstall "X Window Server";
fi


