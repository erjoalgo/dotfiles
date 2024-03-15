#!/bin/bash

set -euo pipefail

while getopts "hli:b" OPT; do
    case ${OPT} in
    l)
        IS_LOGITECH=true
        ;;
    b)
        IS_BT=true
        KEYBOARD_ID=0005:04E8:7021.0012
        ;;
    i)
        KEYBOARD_ID=${OPTARG}*
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))

export DISPLAY=${DISPLAY:-:$(ls /tmp/.X11-unix | tr -d 'X')}
export XAUTHORITY=${XAUTHORITY:-${HOME}/.Xauthority}

echo "KEYBOARD_ID is ${KEYBOARD_ID:-}"

for CAND in ~/.xmodmap/{${KEYBOARD_ID:-},$(hostname),default}.xmodmap; do
    if test -e "${CAND}"; then
        echo "loading xmodmap file: ${CAND}"
        xmodmap "${CAND}"
        sleep 1
        xmodmap "${CAND}"
        sleep 1
        xmodmap "${CAND}"
        break
    else
        echo "file does not exist: ${CAND}"
    fi
done

xset r rate 170 50 # kbd delay, repeat rate
xset m 10 1 # mouse accel, thresh

if test -n "${IS_LOGITECH:-}"; then
    # run this last as it may fail
    sudo $(which solaar) config 1 fn-swap off
fi
