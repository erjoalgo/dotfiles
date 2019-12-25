#!/bin/bash -x

set -euo pipefail

URL=http://prdownloads.sourceforge.net/sbcl/sbcl-1.5.9-source.tar.bz2

BASE=$(basename "${URL}")
DIR_PREFIX=$(grep -o "sbcl-[0-9.]*"  <<< "${BASE}")

TOP=${HOME}/src
mkdir -p ${TOP}
cd ${TOP}

function find-dir  {
    find . -name "${DIR_PREFIX}*" -type d |  \
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

sh make.sh --fancy
sudo ./install.sh
sudo apt-get install -y make rlwrap
