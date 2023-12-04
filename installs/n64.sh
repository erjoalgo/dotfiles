#!/bin/bash -x

set -euo pipefail

sudo apt-get install -y mupen64plus-qt

INI=/usr/share/games/mupen64plus/InputAutoCfg.ini

sudo sed -i 's/^[[]Keyboard]$/[Original Keyboard]/g' "${INI}"

sudo insert-text-block '; c7ffec9c-8d9c-4044-a8e9-9b736d0bba2e-nintendo-64-keyboard' \
     -b ${INI} <<EOF
[Keyboard]
plugged = True
mouse = False
DPad R = key(100)
DPad L = key(97)
DPad D = key(115)
DPad U = key(119)
Start = key(13)
Z Trig = key(304)
B Button = key(99)
A Button = key(120)
C Button R = key(103)
C Button L = key(100)
C Button D = key(102)
C Button U = key(114)
R Trig = key(122)
L Trig = key(97)
Mempak switch = key(44)
Rumblepak switch = key(46)
X Axis = key(276,275)
Y Axis = key(273,274)
EOF

sudo chgrp input $(which mupen64plus)
sudo chmod g+s $(which mupen64plus)
