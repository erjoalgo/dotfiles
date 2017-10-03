#!/bin/bash -x

if command -v tor; then
    exit 0
fi

CODENAME=$(lsb_release -c | cut -d: -f2 | tr -d '[:space:]')
LINE="deb     http://deb.torproject.org/torproject.org ${CODENAME} main"

APT_SOURCES=/etc/apt/sources.list

if ! grep -F "${LINE}" ${APT_SOURCES}; then
    echo "${LINE}" | sudo tee -a ${APT_SOURCES} || exit ${LINENO}
fi


gpg --keyserver keys.gnupg.net --recv A3C4F0F979CAA22CDBA8F512EE8CBC9E886DDD89 || exit ${LINENO}
gpg --export A3C4F0F979CAA22CDBA8F512EE8CBC9E886DDD89 | sudo apt-key add - || exit ${LINENO}

sudo apt-get update || exit ${LINENO}
sudo apt-get install -y deb.torproject.org-keyring tor

cat <<EOF >> ~/.torrc
TestSocks 1
SafeSocks 1
EOF
