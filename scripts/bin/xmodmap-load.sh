#!/bin/bash -x


exec >> /tmp/udev.log
exec 2>&1

echo loading
{
    set -euo pipefail
    EALFONSO_HOME=$(echo /home/e*alfonso)
    EALFONSO_USER=$(basename ${EALFONSO_HOME})
    export XAUTHORITY=${EALFONSO_HOME}/.Xauthority
    export DISPLAY=:0

    xmessage hola -timeout 1
    UDEV_RULE=/etc/udev/rules.d/logitech-keyboard.rules
    cat<<EOF | sudo tee "${UDEV_RULE}"
ACTION="bind", ATTRS{idVendor}=="046d", ATTRS{idProduct}=="c52b", RUN+="/tmp/test.sh"
EOF

    if test $(whoami) != ${EALFONSO_USER}; then
        su ${EALFONSO_USER}
    fi

    for CAND in ~/.xmodmap/{$(hostname),default}.xmodmap; do
        if test -e "${CAND}"; then
            xmodmap "${CAND}"
            break
        fi
    done

    xset r rate 170 50 # kbd delay, repeat rate
    xset m 10 1 # mouse accel, thresh

    env
    echo "pid: $$?" $(whoami)
    # run this last as it may fail
    for _ in $(seq 10); do
        if sudo $(which solaar) config 1 fn-swap off; then
            break
        fi
        # echo "failed, retrying..."
        xmessage "failed, retrying..." -timeout 1
        # sleep 1
    done
    echo "done"
    xmessage done -timeout 1
}
