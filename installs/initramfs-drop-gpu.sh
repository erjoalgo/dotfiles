#!/bin/bash

set -euo pipefail

# Leave GPU drivers and their firmware out of every initramfs. They're
# most of phantom's 139 MB MODULES=most image (amdgpu and nvidia
# firmware alone are 140 MB unpacked), which is too big for its 456 MB
# /boot. Mounting root doesn't need them: the GPU driver loads from the
# root filesystem right after, firmware included.
# The cost is no early KMS: the console stays at the firmware's
# resolution until then (a LUKS passphrase prompt would too).
#
# Drivers and firmware go together: a GPU driver loaded from the
# initramfs without its firmware would fail to initialize.

HOOK=/etc/initramfs-tools/hooks/drop-gpu

sudo tee "${HOOK}" >/dev/null <<'EOT'
#!/bin/sh
# Installed by dotfiles' installs/initramfs-drop-gpu.sh.
case "$1" in
    prereqs)
        exit 0
        ;;
esac
# mkinitramfs runs depmod after the hooks, so the removed modules drop
# out of modules.dep too.
for DIR in "${DESTDIR}/usr/lib/firmware" "${DESTDIR}/lib/firmware"; do
    rm -rf "${DIR}/amdgpu" "${DIR}/nvidia" "${DIR}/i915" "${DIR}/xe" "${DIR}/radeon"
done
rm -rf "${DESTDIR}${MODULESDIR}/kernel/drivers/gpu"
EOT
sudo chmod 755 "${HOOK}"

sudo update-initramfs -u -k all
ls -la /boot/initrd.img-*
df -h /boot
