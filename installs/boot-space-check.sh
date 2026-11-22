#!/bin/bash -x

set -euo pipefail

# Run bin/boot-space-check.sh daily as a user timer, so its alert reaches
# the desktop session.

CHECK=$(realpath "$(dirname "$0")/../bin/boot-space-check.sh")
DIR=${HOME}/.config/systemd/user
mkdir -p "${DIR}"

cat > "${DIR}/boot-space-check.service" <<EOT
[Unit]
Description=Alert when /boot is nearly full or dpkg is half-configured

[Service]
Type=oneshot
Environment=DISPLAY=:0
ExecStart=${CHECK}
EOT

cat > "${DIR}/boot-space-check.timer" <<'EOT'
[Unit]
Description=Daily /boot space and dpkg check

[Timer]
OnCalendar=daily
Persistent=true
RandomizedDelaySec=1h

[Install]
WantedBy=timers.target
EOT

systemctl --user daemon-reload
systemctl --user enable --now boot-space-check.timer
