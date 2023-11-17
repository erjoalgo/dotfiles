#!/bin/bash -x

set -euo pipefail

CODENAME=$(lsb_release -c | cut -d: -f2 | tr -d '[:space:]')
sudo tee /etc/apt/sources.list.d/torproject.list <<EOF
deb     http://deb.torproject.org/torproject.org ${CODENAME} main
deb-src     http://deb.torproject.org/torproject.org ${CODENAME} main
EOF

TOR_KEY_URL=https://deb.torproject.org/torproject.org/A3C4F0F979CAA22CDBA8F512EE8CBC9E886DDD89.asc
KEYRING_FILE=/usr/share/keyrings/tor-archive-keyring.gpg

if ! test -e "${KEYRING_FILE}"; then
    curl ${TOR_KEY_URL} \
         gpg --dearmor | \
        sudo tee "${KEYRING_FILE}"
fi

wget -q "${TOR_KEY_URL}" -O- |  \
    sudo apt-key add -

sudo apt-get update
sudo apt-get install -y deb.torproject.org-keyring tor

insert-text-block '# 2f798a9e-bef5-401e-af6e-8597461114b8-tor-client' \
                  ${HOME}/.torrc<<EOF
TestSocks 1
SafeSocks 1
EOF

sudo service tor start
