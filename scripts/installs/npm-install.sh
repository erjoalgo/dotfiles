#!/bin/bash -x

set -euo pipefail
mkdir -p ~/src && cd ~/src
test -d node || git clone git://github.com/ry/node.git
cd node
./configure
make
sudo make install
