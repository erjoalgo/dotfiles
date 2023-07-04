#!/bin/bash -x

set -euo pipefail

cat <<EOF | sudo tee /etc/apt/sources.list.d/virtualbox.list
deb [arch=amd64] https://download.virtualbox.org/virtualbox/debian buster contrib
EOF

for KEY in  \
    \
    https://www.virtualbox.org/download/oracle_vbox.asc \
        https://www.virtualbox.org/download/oracle_vbox_2016.asc \
    ; do
    wget -q  -O- ${KEY_URL} | sudo apt-key add -
done

sudo apt-get -y update
sudo apt-get install -y virtualbox-6.1

vboxmanage setextradata global GUI/SuppressMessages "all"
