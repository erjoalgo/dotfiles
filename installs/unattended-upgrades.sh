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

# 4. Make sure the timers are on
sudo systemctl enable --now apt-daily.timer apt-daily-upgrade.timer

# 5. Verify what apt actually sees
sudo apt-config dump | grep -E 'Periodic::(Update-Package-Lists|Unattended-Upgrade) '
