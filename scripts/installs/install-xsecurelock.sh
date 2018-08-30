#!/bin/bash -x

set -euo pipefail

if command -v xsecurelock; then
    exit 0
fi

sudo apt-get install -y libxmu-dev libpam-dev xautolock

URL=https://github.com/google/xsecurelock

cd ${HOME}/git

test -d xsecurelock || git clone ${URL}

cd xsecurelock

sh autogen.sh

SERVICE_NAME=""

for CAND in \
    common-auth \
        common-auth-screensaver \
        xscreensaver \
    ; do
    if test -e /etc/pam.d/${CAND}; then
        SERVICE_NAME=${CAND}
        break
    fi
done

./configure --with-pam-service-name=${SERVICE_NAME}
make
sudo make install
