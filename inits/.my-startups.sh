#!/bin/bash -x

PID_FILE="/tmp/.my_startups.pid"
echo $$> ${PID_FILE}
{
    sleep 30
    while ls ${PID_FILE}; do
	notify-send "my-startups.sh has not finished!"
	sleep 30
    done
}&

xbacklight -set 70
xset r rate 170 50 #kbd delay, repeat rate
xset m 10 1 #mouse accel, thresh
sudo modprobe -r pcspkr


/usr/lib/notify-osd/notify-osd &

emacs &
firefox &
x-terminal-emulator &
##/usr/bin/keynav &


command -v keynav  && keynav &

INTERNET_TEST="curl http://erjoalgo.com:7036"

if ! ${INTERNET_TEST}; then
    sudo wifi -y -t ac
    if ! ${INTERNET_TEST}; then
	sudo dhclient -v eth2
    fi
fi


XSCRIPTS="${HOME}/x-scripts.sh"

if test -e "${XSCRIPTS}"; then
    echo "running x scripts at: ${XSCRIPTS}"
    ${XSCRIPTS} &#these should not block
fi

rm ${PID_FILE}

notify-send "my_startups.sh done"
