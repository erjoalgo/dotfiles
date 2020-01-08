#!/bin/bash -x

set -euo pipefail

CODENAME=$(lsb_release -c | cut -d: -f2 | tr -d '[:space:]')
sudo tee /etc/apt/sources.list.d/torproject.list <<EOF
deb     http://deb.torproject.org/torproject.org ${CODENAME} main
deb-src     http://deb.torproject.org/torproject.org ${CODENAME} main
EOF

curl https://deb.torproject.org/torproject.org/A3C4F0F979CAA22CDBA8F512EE8CBC9E886DDD89.asc \
    | gpg --import
gpg --export A3C4F0F979CAA22CDBA8F512EE8CBC9E886DDD89 | sudo apt-key add -

sudo apt-get update
sudo apt-get install -y deb.torproject.org-keyring tor

cat <<EOF >> ~/.torrc
TestSocks 1
SafeSocks 1
EOF
sudo service tor start
