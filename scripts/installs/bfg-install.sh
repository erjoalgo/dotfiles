#!/bin/bash -x

set -euo pipefail

if ! which bfg; then
    mkdir -p ~/src
    cd ~/src
    URL=http://repo1.maven.org/maven2/com/madgag/bfg/1.13.0/bfg-1.13.0.jar
    BASE=$(basename ${URL})
    test -e ${BASE} || wget ${URL}

    LINKNAME=bfg make-jar-executable-and-link.sh ${BASE}
    which bfg
fi
