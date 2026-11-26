#!/bin/bash -x

set -euo pipefail

LOG=/var/log/bookworm-upgrade.log

sudo touch ${LOG}
sudo chmod a+w ${LOG}

exec > >(tee -a ${LOG}) 2>&1

MIN_FREE_GB=${MIN_FREE_GB:-5}
for DIR in / /var/cache/apt/archives; do
    FREE_GB=$(df --output=avail -BG "${DIR}" | tail -1 | tr -dc 0-9)
    if test "${FREE_GB}" -lt "${MIN_FREE_GB}"; then
        echo "only ${FREE_GB}G free on ${DIR}, need ${MIN_FREE_GB}G (MIN_FREE_GB)" >&2
        exit 1
    fi
done

dpkg --get-selections '*'

sudo apt list '?obsolete'

sudo apt list '?config-files'
# sudo apt purge '?config-files'

sudo apt autoremove -y
sudo apt clean

sudo apt-get update
sudo apt-get upgrade --without-new-pkgs -y
sudo apt-get autoremove -y

sudo find /etc/apt/ -type f -name '*.list' -exec sed -i 's/bookworm/trixie/g' {} \;
# packages.microsoft.com also has the release number in the path
sudo find /etc/apt/ -type f -name '*.list' -exec sed -i 's|/debian/12/|/debian/13/|g' {} \;

sudo apt-get update
sudo apt-get upgrade --without-new-pkgs -y
sudo apt-get full-upgrade -y

sudo find /etc/apt/ -name '*.bak'  -exec rm {} +

sudo apt-get update

sudo apt-get install -y linux-image-amd64

sudo apt purge -y '~c'
echo "packages no longer in any repo; review, then 'sudo apt purge ~o' if none are wanted:"
sudo apt list '~o'
