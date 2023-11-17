#!/bin/bash

CURRENT_BACKLIGHT=$(xbacklight -get)
if test $? -eq 0 &&  expr ${CURRENT_BACKLIGHT} \> 70; then
    xbacklight -set 70
fi

xset r rate 170 50 #kbd delay, repeat rate
xset m 10 1 #mouse accel, thresh

if command -v keynav; then
    keynav &
fi
