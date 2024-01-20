#!/bin/bash -x

set -euo pipefail

sudo apt-get install -y qemu-system libvirt-daemon-system
sudo apt-get install -y virt-manager aqemu
sudo adduser $(whoami) libvirt
sudo -u $(whoami) virsh list --all
