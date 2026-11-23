#!/bin/bash -x

set -euo pipefail

# Build each initramfs with only the modules this machine needs
# (MODULES=dep) instead of nearly all of them (Debian's MODULES=most),
# so a small /boot fits the kernels apt keeps. The trade-off: booting
# this disk on different hardware needs the initramfs rebuilt there.

# conf.d is read after initramfs.conf, so this overrides it.
sudo tee /etc/initramfs-tools/conf.d/modules-dep >/dev/null <<'EOT'
MODULES=dep
EOT

sudo update-initramfs -u -k all
ls -la /boot/initrd.img-*
df -h /boot
