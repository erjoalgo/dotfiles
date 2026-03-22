#!/bin/bash -x

ir-remote.py -bTCL_BRIGHTNESS_DOWN,LG_BRIGHTNESS_DOWN &

redshift -l28.562871:-81.210339 -ov -PO 500

pgrep picom || invert-colors-toggle.sh

