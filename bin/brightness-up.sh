#!/bin/bash -x

set -euo pipefail

ir-remote.py -bTCL_BRIGHTNESS_UP,LG_BRIGHTNESS_UP

redshift -l28.562871:-81.210339 -ov -PO 6000

pkill picom || true
