#!/bin/bash

xbacklight -set 70
xset r rate 170 50 #kbd delay, repeat rate
xset m 10 1 #mouse accel, thresh
sudo modprobe -r pcspkr
/usr/lib/notify-osd/notify-osd &
