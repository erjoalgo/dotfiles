#!/bin/bash -x

set -euo pipefail

LOG=/var/log/bookworm-upgrade.log
sudo touch ${LOG}
sudo chmod a+w ${LOG}

exec > >(tee ${LOG}) 2>&1

sudo apt-get update
sudo apt-get upgrade -y
sudo apt-get autoremove

sudo sed -i 's/bullseye/bookworm/g' /etc/apt/sources.list
sudo apt-get update
sudo apt-get upgrade --without-new-pkgs -y
sudo apt-get full-upgrade -y


