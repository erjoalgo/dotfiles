#!/bin/bash

SPEC=${1} && shift

while getopts "ha:" OPT; do
    case ${OPT} in
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))

if test ${SPEC} = 'ignore'; then
    CONTENTS=$(cat<<EOF
HandleLidSwitch=ignore
HandlePowerKey=ignore
# HandleLidSwitch=suspend
# HandleLidSwitchExternalPower=suspend
# HandleLidSwitchDocked=suspend
# LidSwitchIgnoreInhibited=yes
EOF
)
    # pkill xautolock
elif test ${SPEC} = 'respect'; then
    CONTENTS=$(cat<<EOF
# HandleLidSwitch=ignore
# HandlePowerKey=ignore
HandleLidSwitch=suspend
HandleLidSwitchExternalPower=suspend
HandleLidSwitchDocked=suspend
LidSwitchIgnoreInhibited=yes
EOF
)
    # xautolock
else
    echo "usage: ignore-lid-close.sh [respect|ignore]"
    exit ${LINENO}
fi

sudo insert-text-block \
     '# 03c99349-27b7-4d7e-a414-a3ef9b85acbc-ignore-lid-close-toggle'  \
     /etc/systemd/logind.conf <<< "${CONTENTS}"

sudo service systemd-logind restart
