#!/bin/bash -x

set -euo pipefail

if ! which sbcl | grep local || test -n "${FORCE:-}"; then
    # URL=http://prdownloads.sourceforge.net/sbcl/sbcl-2.2.0-source.tar.bz2
    URL=https://github.com/sbcl/sbcl/archive/refs/tags/sbcl-2.5.5.tar.gz

    BASE=$(basename "${URL}")
    DIR_PREFIX=$(grep -o "sbcl-[0-9.]*[0-9]"  <<< "${BASE}")

    TOP=${HOME}/src
    mkdir -p ${TOP}
    cd ${TOP}

    function find-dir  {
        find . -name "*${DIR_PREFIX}*" -type d |  \
            tr -d '\n'
    }

    DIR=$(find-dir)

    if ! test -d "${DIR}"; then
        test -e ${BASE} || wget "${URL}"
        test -e ${BASE}
        tar -C ~/src/ -axvf ${BASE}
        DIR=$(find-dir)
    fi

    test -d "${DIR}"

    cd "${DIR}"

    sudo apt-get install -y zlib1g-dev
    sudo apt-get install -y sbcl
    sh make.sh --fancy
    sudo apt-get purge -y sbcl
    sudo sh ./install.sh
fi

sudo apt-get install -y make rlwrap

QL_SETUP="${HOME}/quicklisp/setup.lisp"
if ! test -f "${QL_SETUP}"; then
    cd /tmp
    curl -O https://beta.quicklisp.org/quicklisp.lisp
    curl -O https://beta.quicklisp.org/quicklisp.lisp.asc
    gpg --verify quicklisp.lisp.asc quicklisp.lisp || true
    sbcl --load quicklisp.lisp --non-interactive --eval  \
	'(quicklisp-quickstart:install)'
    rm quicklisp.lisp{,.asc}
fi

sbcl --non-interactive --load ${QL_SETUP} --eval "(ql:quickload 'quicklisp-slime-helper)"

SBCLRC="${HOME}/.sbclrc"
# the below command is interactive. it is simpler to insert the text to load
# sbcl --load "${QL_SETUP}" --non-interactive --eval '(ql:add-to-init-file)' < /dev/null

insert-text-block ";;; 2a00bf58-9854-4512-8e13-a85409493a54-ql:add-to-init-file-manual" \
	          "${SBCLRC}"<<EOF
  ;;; The following lines added by ql:add-to-init-file:
  #-quicklisp
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                         (user-homedir-pathname))))
    (when (probe-file quicklisp-init)
      (load quicklisp-init)))
EOF
