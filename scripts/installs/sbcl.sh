#!/bin/bash -x

set -euo pipefail

URL=http://prdownloads.sourceforge.net/sbcl/sbcl-1.4.15-x86-64-linux-binary.tar.bz2

BASE=$(basename "${URL}")
DIR_PREFIX=$(grep -o "sbcl[0-9.-]*"  <<< "${BASE}")

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

sudo ./install.sh
