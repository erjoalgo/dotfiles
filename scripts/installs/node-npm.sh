#!/bin/bash -x

set -euo pipefail

URL=https://nodejs.org/dist/v6.11.4/node-v6.11.4-linux-x64.tar.gz
SRC_DIR=${HOME}/src
FNAME=$(basename ${URL})
DNAME=${SRC_DIR}/$(basename ${FNAME} .tar.gz)

cd /tmp
test -e ${FNAME} || wget ${URL} -o /dev/stderr
test -d ${DNAME} || tar -C ${SRC_DIR} -xavf ${FNAME}

PROFILE_FILE=/etc/profile.d/node-env.sh
which insert-text-block
sudo $(which insert-text-block) \
     '# 4e96a5a7-d75e-4cfb-9d80-85a5e2ea704e-nodejs-path-addition'  \
     ${PROFILE_FILE} <<EOF
export PATH=\${PATH}:${DNAME}/bin
EOF

source ${PROFILE_FILE}
which node
which npm
