#!/bin/bash
echo "running xmodmapstart"
[ -e /tmp/xmodmap.pke ] || xmodmap -pke > /tmp/xmodmap.pke
xmodmap ~/repos/stumpwm/xmodmap/.xmodmaprc
xmodmap ~/repos/stumpwm/xmodmap/.xmodmaprc
xset r rate 190 50
xset m 10 1
echo "ran xmodmapstart"
