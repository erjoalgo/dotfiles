#!/bin/bash -x

set -euo pipefail

DEVICE=$(adb-device-select)
scrcpy -s "${DEVICE}" --shortcut-mod=lsuper -S
