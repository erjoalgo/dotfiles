#!/bin/bash -x

set -euo pipefail

sudo systemctl unmask sleep.target suspend.target hibernate.target hybrid-sleep.target

CONF=/etc/systemd/sleep.conf.d/suspend-then-hibernate
sudo mkdir -p $(dirname "${CONF}")

sudo insert-text-block '# 54e80f50-57a3-4daf-94b7-17e0af86eac9-suspend-then-hibernate'  \
                  "${CONF}"<<EOF
[Sleep]
AllowSuspend=yes
AllowHibernation=yes
AllowSuspendThenHibernate=no
#AllowHybridSleep=yes
SuspendMode=suspend
#SuspendState=mem standby freeze
HibernateMode=shutdown
#HibernateState=disk
#HybridSleepMode=suspend platform shutdown
#HybridSleepState=disk
HibernateDelaySec=10
#SuspendEstimationSec=60min
EOF

ignore-lid-close.sh respect

install-systemd-service.sh suspend-sedation<<"EOF"
[Unit]
Description=Hibernate after suspend
Documentation=https://bbs.archlinux.org/viewtopic.php?pid=1420279#p1420279
Documentation=https://bbs.archlinux.org/viewtopic.php?pid=1574125#p1574125
Documentation=https://wiki.archlinux.org/index.php/Power_management
Documentation=http://forums.debian.net/viewtopic.php?f=5&t=129088
Documentation=https://wiki.debian.org/SystemdSuspendSedation
Conflicts=hibernate.target hybrid-sleep.target
Before=sleep.target
StopWhenUnneeded=true

[Service]
Type=oneshot
RemainAfterExit=yes
Environment="ALARM_SEC=60"
Environment="WAKEALARM=/sys/class/rtc/rtc0/wakealarm"

ExecStart=/usr/sbin/rtcwake --seconds $ALARM_SEC --auto --mode no
ExecStop=/bin/sh -c '\
ALARM=$(cat $WAKEALARM); \
NOW=$(date +%%s); \
if [ -z "$ALARM" ] || [ "$NOW" -ge "$ALARM" ]; then \
  echo "suspend-sedation: Woke up - no alarm set. Hibernating..."; \
  systemctl hibernate; \
else \
  echo "suspend-sedation: Woke up before alarm - normal wakeup."; \
  /usr/sbin/rtcwake --auto --mode disable; \
fi \
'

[Install]
WantedBy=sleep.target
WantedBy=suspend.target
EOF

if ! test -e /swap.file; then
    sudo $(which mkswap-file.sh)
fi

for DIR in  \
    /etc/systemd/system/systemd-logind.service.d/ \
                                                /etc/systemd/system/systemd-hibernate.service.d/; do
    sudo mkdir -p "${DIR}"
    sudo tee "${DIR}/override.conf" <<EOF
[Service]
Environment=SYSTEMD_BYPASS_HIBERNATION_MEMORY_CHECK=1
EOF
done

sudo systemctl daemon-reload
sudo service systemd-logind restart
