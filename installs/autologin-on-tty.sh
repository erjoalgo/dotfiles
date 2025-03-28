#!/bin/bash -x

set -euo pipefail

sudo insert-text-block '# Zss7UaEgcFtP1T8JPS7h77vOaQlYDR3H-enable-autologin' \
     /etc/systemd/logind.conf <<EOF
NAutoVTs=6
ReserveVT=6
EOF

sudo insert-text-block \
     '# bZkpjy56EU9dBbdNDTk0zmMVGWJLzK9e-ssh' \
     "${HOME}/.ssh/config" <<EOF
XAuthLocation /opt/X11/bin/xauth
EOF

install-systemd-service.sh -o getty@tty1 <<EOF
[Service]
ExecStart=
ExecStart=-/sbin/agetty --autologin "${USER}" -o '-p -f ${USER}' %I $TERM
EOF
