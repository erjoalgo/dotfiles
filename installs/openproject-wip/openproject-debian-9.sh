#!/bin/bash -x

set -euo pipefail

wget -qO- https://dl.packager.io/srv/opf/openproject/key | sudo apt-key add -
sudo apt-get install -y apt-transport-https

sudo wget -O /etc/apt/sources.list.d/openproject.list \
     https://dl.packager.io/srv/opf/openproject/stable/9/installer/debian/9.repo

sudo apt-get update
sudo apt-get install -y openproject
