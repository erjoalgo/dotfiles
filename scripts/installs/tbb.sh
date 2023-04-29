#!/bin/bash -x

set -euo pipefail

URL=https://www.torproject.org/dist/torbrowser/12.0.5/tor-browser-linux64-12.0.5_ALL.tar.xz

install-from-source -u "${URL}" -n -d ${HOME}/src

PROFILE_ENV=${HOME}/.profile-env

insert-text-block  \
    '# b5b9774a-9561-4cd8-ba55-12f9ab3f495e-add-tor-browser'  \
    ${PROFILE_ENV} <<EOF
export PATH+=:${HOME}/src/tor-browser/Browser/
EOF

source ${PROFILE_ENV}
echo ${PATH}

which start-tor-browser

