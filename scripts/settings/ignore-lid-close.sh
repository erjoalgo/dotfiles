#!/bin/bash

sudo insert-text-block \
     '# 03c99349-27b7-4d7e-a414-a3ef9b85acbc-ignore-lid-close-toggle'  \
     /etc/systemd/logind.conf <<EOF
HandleLidSwitch=ignore
HandlePowerKey=ignore

# HandleLidSwitch=suspend
# HandleLidSwitchExternalPower=suspend
# HandleLidSwitchDocked=suspend
# LidSwitchIgnoreInhibited=yes
EOF

sudo service systemd-logind restart
