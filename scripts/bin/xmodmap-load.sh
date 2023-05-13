#!/bin/bash -x

set -euo pipefail

while getopts "hl" OPT; do
    case ${OPT} in
    l)
        IS_LOGITECH=true
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))

export DISPLAY=:$(ls /tmp/.X11-unix | tr -d 'X')
export XAUTHORITY=~/.Xauthority

for CAND in ~/.xmodmap/{$(hostname),default}.xmodmap; do
    if test -e "${CAND}"; then
        xmodmap "${CAND}"
        break
    fi
done

xset r rate 170 50 # kbd delay, repeat rate
xset m 10 1 # mouse accel, thresh

if test -n "${IS_LOGITECH:-}"; then
    # run this last as it may fail
    sudo $(which solaar) config 1 fn-swap off
fi
