#!/bin/bash -x

set -euo pipefail

# exec >> /tmp/xmodmap-load-${USER}.log
# exec 2>&1

{
    echo $0 args
    echo ${*}
    date
    env
    echo
    echo
}

NON_ROOT_USER=$(ls /home)
if ! test "${USER}" = "${NON_ROOT_USER}" && ! su "${NON_ROOT_USER}" < /dev/null; then
    echo "warn: unable to switch user"
    exit 0
fi

export DISPLAY=:0.0
export XAUTHORITY=~/.Xauthority

for CAND in ~/.xmodmap/{$(hostname),default}.xmodmap; do
    if test -e "${CAND}"; then
        xmodmap "${CAND}"
        break
    fi
done

xset r rate 170 50 # kbd delay, repeat rate
xset m 10 1 # mouse accel, thresh

# run this last as it may fail
sudo solaar config 1 fn-swap off
