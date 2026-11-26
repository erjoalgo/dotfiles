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

NEWEST_KERNEL=$(ls /boot/vmlinuz-* | sed 's|^/boot/vmlinuz-||' | sort -V | tail -1)
if test "$(uname -r)" != "${NEWEST_KERNEL}"; then
    echo "running kernel $(uname -r) but ${NEWEST_KERNEL} is installed: reboot first" >&2
    exit 1
fi

dpkg --get-selections '*'

sudo apt list '?obsolete'

sudo apt list '?config-files'
# sudo apt purge '?config-files'

sudo apt autoremove -y
sudo apt clean

sudo apt-get update
sudo apt-get upgrade --without-new-pkgs -y
sudo apt-get autoremove -y

# packages.microsoft.com has the release number in the path and signs
# debian/13 with a newer key, both shipped by its debian 13 config package
if dpkg -s packages-microsoft-prod >/dev/null 2>&1; then
    MS_DEB=$(mktemp --suffix=.deb)
    curl -fsSL -o "${MS_DEB}" https://packages.microsoft.com/config/debian/13/packages-microsoft-prod.deb
    sudo dpkg -i --force-confnew "${MS_DEB}"
    rm "${MS_DEB}"
fi

sudo find /etc/apt/ -type f -name '*.list' -exec sed -i 's/bookworm/trixie/g' {} \;

sudo apt-get update
sudo apt-get upgrade --without-new-pkgs -y
sudo apt-get full-upgrade -y

sudo find /etc/apt/ -name '*.bak'  -exec rm {} +

sudo apt-get update

sudo apt-get install -y linux-image-amd64

sudo apt purge -y '~c'
echo "packages no longer in any repo; review, then 'sudo apt purge ~o' if none are wanted:"
sudo apt list '~o'
