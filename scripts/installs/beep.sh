#!/bin/bash -x

set -euo pipefail

sudo apt-get install -y beep
sudo groupadd -f beep
sudo usermod -a -G beep ${USER}

sudo insert-text-block '# 618075ff-72c0-4113-a54c-3b81b180b5b4-pc-spkr'  \
     /usr/lib/udev/rules.d/70-pcspkr-beep.rules<<EOF
ACTION=="add", SUBSYSTEM=="input", ATTRS{name}=="PC Speaker", ENV{DEVNAME}!="", GROUP="beep", MODE="0620"
EOF
sudo rmmod pcspkr
sudo modprobe pcspkr
sudo udevadm control --reload

sudo insert-text-block '# fa4889aa-5bdb-488f-a862-05f95b93ca3b-beep-on-successful-ssh-login' \
    "/etc/ssh/sshd_config"  \
    <<EOF
$(which beep)
EOF
