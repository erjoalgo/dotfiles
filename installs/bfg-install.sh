#!/bin/bash -x

set -euo pipefail

if ! which bfg; then
    mkdir -p ~/src
    cd ~/src
    URL=https://repo1.maven.org/maven2/com/madgag/bfg/1.14.0/bfg-1.14.0.jar
    BASE=$(basename ${URL})
    test -e ${BASE} || wget ${URL}

    LINKNAME=bfg make-jar-executable-and-link.sh ${BASE}
    which bfg
fi
