#!/bin/bash -x

set -euo pipefail

URL=${1:-http://download.virtualbox.org/virtualbox/5.1.2/VBoxGuestAdditions_5.1.2.iso}

mkdir -p ~/Downloads && cd ~/Downloads
BASE=$(basename ${URL})
test -e ${BASE} || wget ${URL}

sudo yum update -y
sudo yum install -y dkms gcc make kernel-devel bzip2 binutils patch libgomp glibc-headers glibc-devel kernel-headers


# mount /dev/scd0 /media/cdrom
sudo mkdir -p /media/cdrom
sudo umount /media/cdrom
sudo mount ${BASE} /media/cdrom
sudo sh /media/cdrom/VBoxLinuxAdditions.run
