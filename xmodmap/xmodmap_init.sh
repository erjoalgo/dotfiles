#!/bin/bash

#save original mapping
[ -e /tmp/xmodmap.pke ] || xmodmap -pke > /tmp/xmodmap.pke


#kbd repeat rate, delay
xset r rate 190 50

#mouse accel, thresh
xset m 10 1
