#!/bin/bash -x

set -euo pipefail

LOG=/var/log/bookworm-upgrade.log
sudo touch ${LOG}
sudo chmod a+w ${LOG}

exec > >(tee ${LOG}) 2>&1

sudo apt-get update
sudo apt-get upgrade -y
sudo apt-get autoremove -y

sudo find /etc/apt/ -type f -name '*.list' -exec sed -i 's/bullseye/bookworm/g' {} \;

sudo apt-get update
sudo apt-get upgrade --without-new-pkgs -y
sudo apt-get full-upgrade -y

sudo find /etc/apt/ -type f -exec  \
     sed -i 's/non-free\( \|$\)/non-free-firmware /g' {} \;

sudo find /etc/apt/ -name '*.bak'  -exec rm {} +

sudo apt-get update

sudo apt-get install -y linux-image-amd64

sudo apt purge -y '~c'
sudo apt purge -y '~o'
