#!/bin/bash -x

set -euo pipefail

sudo insert-text-block '# Zss7UaEgcFtP1T8JPS7h77vOaQlYDR3H-enable-autologin' \
     /etc/systemd/logind.conf <<EOF
NAutoVTs=6
EOF

sudo insert-text-block \
     '# bZkpjy56EU9dBbdNDTk0zmMVGWJLzK9e-ssh' \
     "${HOME}/.ssh/config" <<EOF
XAuthLocation /opt/X11/bin/xauth
EOF

sudo insert-text-block \
     '# yTyIZrilAW59XgITTlNwLp3VdhMn9k7R-enable-sysrq' \
     /etc/sysctl.d/99-sysctl.conf<<EOF
kernel.sysrq = 1
EOF

