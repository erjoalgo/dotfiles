#!/bin/bash -x

exec 2> "${HOME}/logs/my_startups/my_startup"  # send stderr from rc.local to a log file
exec 1>&2                      # send stdout to the same log file

echo $$> "${HOME}/.my_startups.pid"
{
    sleep 30
    while ls "${HOME}/.my_startups.pid"; do
	notify-send "my_startups.sh has not finished!"
	sleep 30
    done
}&
xset b off
xset m 10 1
"${HOME}/.xmodmap_init.sh" &> "${HOME}/logs/my_startups/xmodmap"
xmodmap -e 'keycode 135 = Hyper_L'
xmodmap -e 'keycode 105 = Hyper_L'
xmodmap -e 'remove control = Hyper_L'
##/usr/bin/keynav &
if ! ps ax|grep emacs|grep -v grep; then
    emacs --real-session & &>"${HOME}/logs/my_startups/emacs"
fi
x-terminal-emulator &
/usr/lib/notify-osd/notify-osd &
##${HOME}/Projects/bin/mount_ffram
xbacklight -set 70
# "${HOME}/unix_utils/brightnesschange.py" .3 &> "${HOME}/logs/my_startups/brightnesschange"
# firefox -P default &
firefox -P erjoalgo &
sudo modprobe -r pcspkr
# if ! ping 162.228.201.6 -c 3; then
if ! wget -S -O - root.erjoalgo.com/test_online; then
    sudo wifi -y -t ac
fi
if ! wget -S -O - root.erjoalgo.com/test_online; then
    sudo dhclient -v eth2
fi
# pidgin&
rm "${HOME}/.my_startups.pid"
extkeyboard.xmodmap
notify-send "my_startups.sh done"
