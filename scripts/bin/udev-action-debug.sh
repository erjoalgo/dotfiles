#!/bin/bash

# set -euo pipefail

logger "on udev-action-debug.sh"

echo ${0} >> /tmp/hola
echo ${*} >> /tmp/hola
echo "" >> /tmp/hola

# case ${ID_SERIAL_SHORT:-} in
#     07038A9A21244B29)
#         beep -f 880
#         ;;
#     ZY224C7JXT)
#         beep -f 220
#         ;;
#     *)
#         beep -f 1200
#         ;;
# esac

# xmessage "id: ${ID_SERIAL_SHORT:-}"

{ date; echo {*}; env } | tee -a /tmp/udev.log

/home/ealfonso/git/erjoalgo-stumpwmrc/scripts/bin/x-service-curl \
    "/notify" -d "id serial: '${ID_SERIAL_SHORT}'"

# ID_SERIAL_SHORT=07038A9B10244B25
# DEVNAME=/dev/sde1

exit 0
