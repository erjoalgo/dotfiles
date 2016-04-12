#!/bin/bash

sudo apt-get install -y sbcl curl autoconf make || exit ${LINENO}
curl -O http://beta.quicklisp.org/quicklisp.lisp || exit ${LINENO}

TMPLISP=/tmp/ql-load.lisp
cat << EOF > "${TMPLISP}"
(quicklisp-quickstart:install)
(ql:quickload "clx")
(ql:quickload "cl-ppcre")
(ql:quickload "swank")
(ql:quickload "quicklisp-slime-helper")
(ql:add-to-init-file)
(quit)
EOF
sbcl --load quicklisp.lisp --load "${TMPLISP}" || exit ${LINENO}
rm quicklisp.lisp

PROGRAMS="${HOME}/lib"
test -d "${PROGRAMS}" || mkdir "${PROGRAMS}"
cd "${PROGRAMS}"

if ! test -d stumpwm; then
    git clone https://github.com/stumpwm/stumpwm.git || exit ${LINENO}
fi

cd stumpwm

if ! test -f stumpwm; then #the executable
    read -p "confirm make in ${PWD}: "
    ./autogen.sh || exit ${LINENO}
    ./configure || exit ${LINENO} 
    make || exit ${LINENO}
