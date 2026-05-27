#!/bin/bash -x

redshift -l28.562871:-81.210339 -ov -PO 1000

pgrep picom || invert-colors-toggle.sh &


emacsclient-wrapper.sh -e '(redshift-load-dark-theme)' &

irremote -bTCL_BRIGHTNESS_DOWN,LG_BRIGHTNESS_DOWN &

