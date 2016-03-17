#!/bin/bash -x

echo $$> "${HOME}/.my_startups.pid"
{
    sleep 30
    while ls "${HOME}/.my_startups.pid"; do
	notify-send "my_startups.sh has not finished!"
	sleep 30
    done
}&



/usr/lib/notify-osd/notify-osd &

emacs &
x-terminal-emulator &
##/usr/bin/keynav &

#xbacklight -set 70
firefox &

sudo modprobe -r pcspkr

#TODO test this works
INTERNET_TEST="curl http://erjoalgo.com:7036"

if ! ${INTERNET_TEST}; then
    sudo wifi -y -t ac
    
    if ! ${INTERNET_TEST}; then
	sudo dhclient -v eth2
    fi
    
fi



rm "${HOME}/.my_startups.pid"

XSCRIPTS="${HOME}/x-scripts.sh"

if test -e "${XSCRIPTS}"; then
    echo "running x scripts at: ${XSCRIPTS}"
    ${XSCRIPTS}
fi

notify-send "my_startups.sh done"
