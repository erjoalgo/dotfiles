#!/bin/bash -x

set -euo pipefail

LOG=/var/log/bookworm-upgrade.log

sudo touch ${LOG}
sudo chmod a+w ${LOG}

exec > >(tee ${LOG}) 2>&1

dpkg --get-selections '*'

sudo apt list '?obsolete'
sudo apt purge '?obsolete'

sudo apt list '?config-files'
sudo apt purge '?config-files'

sudo apt autoremove -y
sudo apt clean

sudo apt-get update
sudo apt-get upgrade --without-new-pkgs -y
sudo apt-get autoremove -y

sudo find /etc/apt/ -type f -name '*.list' -exec sed -i 's/bookworm/trixie/g' {} \;

sudo apt-get update
sudo apt-get upgrade --without-new-pkgs -y
sudo apt-get full-upgrade -y

sudo find /etc/apt/ -name '*.bak'  -exec rm {} +

sudo apt-get update

sudo apt-get install -y linux-image-amd64

sudo apt purge -y '~c'
sudo apt purge -y '~o'
