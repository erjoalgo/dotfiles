#!/bin/bash -x

set -euo pipefail

# https://unix.stackexchange.com/questions/18435/

URL=${1:-http://download.virtualbox.org/virtualbox/5.1.28/VBoxGuestAdditions_5.1.28.iso}
# http://download.virtualbox.org/virtualbox/5.1.28/
# http://download.virtualbox.org/virtualbox/5.1.28/VirtualBox-5.1-5.1.28_117968_el7-1.x86_64.rpm

mkdir -p ~/Downloads && cd ~/Downloads
BASE=$(basename ${URL})
test -e ${BASE} || wget ${URL}

sudo yum update -y
sudo yum install -y dkms gcc make kernel-devel bzip2 binutils patch libgomp glibc-headers glibc-devel kernel-headers


# mount /dev/scd0 /media/cdrom
sudo mkdir -p /media/cdrom
sudo umount /media/cdrom || true
sudo mount ${BASE} /media/cdrom
sudo sh /media/cdrom/VBoxLinuxAdditions.run
