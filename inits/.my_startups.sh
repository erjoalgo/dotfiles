#!/bin/bash -x

PID_FILE="/tmp/.my_startups.pid"
echo $$> ${PID_FILE}
{
    sleep 30
    while ls ${PID_FILE}; do
	notify-send "my_startups.sh has not finished!"
	sleep 30
    done
}&

/usr/lib/notify-osd/notify-osd &

xbacklight -set 70
sudo modprobe -r pcspkr

# if ! ping 162.228.201.6 -c 3; then
#why not just ping ?
if ! wget -S -O - root.erjoalgo.com/test_online; then
    sudo wifi -y -t ac
fi
if ! wget -S -O - root.erjoalgo.com/test_online; then
    sudo dhclient -v eth2
fi


XSCRIPTS="${HOME}/x-scripts.sh"
if test -e "${XSCRIPTS}"; then
    echo "running x scripts at: ${XSCRIPTS}"
    ${XSCRIPTS} &#these should not block
fi

rm ${PID_FILE}
notify-send "my_startups.sh done"
