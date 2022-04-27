#!/bin/bash -x

set -euo pipefail

sudo insert-text-block '# Zss7UaEgcFtP1T8JPS7h77vOaQlYDR3H-enable-autologin' \
     /etc/systemd/logind.conf<<EOF
NAutoVTs=6
EOF

AUTOLOGIN_CONF=/etc/systemd/system/getty@tty1.service.d/autologin.conf
# autologin on tty1
sudo mkdir -p $(dirname "${AUTOLOGIN_CONF}")

sudo insert-text-block '# e8a6c230-997f-4dd5-9b57-7e3b31ab67bc'  \
     "${AUTOLOGIN_CONF}" <<EOF
[Service]
ExecStart=-/sbin/agetty --autologin "${USER}" %I
EOF

sudo insert-text-block '# bZkpjy56EU9dBbdNDTk0zmMVGWJLzK9e-ssh' \
     "${HOME}/.ssh/config" <<EOF
XAuthLocation /opt/X11/bin/xauth
EOF

sudo insert-text-block '# yTyIZrilAW59XgITTlNwLp3VdhMn9k7R-enable-sysrq' \
                  /etc/sysctl.d/99-sysctl.conf<<EOF
kernel.sysrq = 1
EOF

sudo systemctl enable getty@tty1.service
