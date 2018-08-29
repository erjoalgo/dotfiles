#!/bin/bash

if expr $(xbacklight -get) \> 70; then
    xbacklight -set 70
fi

xset r rate 170 50 #kbd delay, repeat rate
xset m 10 1 #mouse accel, thresh
