#!/bin/bash -x

set -euo pipefail

xmodmap-load.sh -b
ir-remote.py -b TCL_POWER
ir-remote.py -b VIZIO_POWER
