#!/bin/bash -x

set -euo pipefail

URL=http://download.virtualbox.org/virtualbox/5.1.2/VirtualBox-5.1-5.1.2_108956_el7-1.x86_64.rpm
BASE=$(basename ${URL})
test -e ${BASE} || wget ${URL}

sudo yum install -y ${BASE}

