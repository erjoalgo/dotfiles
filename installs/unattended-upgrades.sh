#!/usr/bin/env bash
set -euo pipefail

# 1. Preseed before install (affects fresh installs)
echo 'unattended-upgrades unattended-upgrades/enable_auto_updates boolean true' \
    | sudo debconf-set-selections

# 2. Install non-interactively
sudo DEBIAN_FRONTEND=noninteractive apt-get install -y unattended-upgrades

# 3. Write the config explicitly (covers already-installed machines,
#    and guarantees the final state regardless of postinst behaviour)
sudo tee /etc/apt/apt.conf.d/20auto-upgrades >/dev/null <<'EOF'
APT::Periodic::Update-Package-Lists "1";
APT::Periodic::Unattended-Upgrade "1";
EOF

# Remove superseded kernels after upgrading, so /boot doesn't fill up
# with one more kernel per upgrade (Debian's default, made explicit).
sudo tee /etc/apt/apt.conf.d/52unattended-upgrades-local >/dev/null <<'EOF'
Unattended-Upgrade::Remove-Unused-Kernel-Packages "true";
EOF

# Versioned kernel images marked manual (e.g. left over from a release
# upgrade) are never removed; leave that to the linux-image-<flavor>
# metapackage, which keeps the current kernel installed.
apt-mark showmanual | grep -E '^linux-image-[0-9]' | xargs -r sudo apt-mark auto

# 4. Make sure the timers are on
sudo systemctl enable --now apt-daily.timer apt-daily-upgrade.timer

# 5. Verify what apt actually sees
sudo apt-config dump | grep -E 'Periodic::(Update-Package-Lists|Unattended-Upgrade) '
